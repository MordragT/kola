use std::sync::Arc;

use camino::Utf8Path;
use kola_builtins::BuiltinLexicon;
use std::io;

use kola_ir::print::render_ir;
use kola_lowerer::module::{Program, lower};
use kola_machine::machine::{Ctx, Machine};
use kola_print::{PrintOptions, prelude::*};
use kola_resolver::{prelude::*, print::ResolutionDecorator};
use kola_runtime::heap::Heap;
use kola_span::{Issue, Report, SourceManager};
use kola_syntax::{
    lexer::{TokenOutput, tokenize},
    loc::LocMap,
    parser::{ParseInput, ParseOutput, parse},
    token::TokenPrinter,
};
use kola_tree::{
    print::{Decorators, TreePrinter},
    tree::{Tree, TreeMap},
};
use kola_typer::{
    check::{TypeCheckOutput, type_check},
    print::TypeDecorator,
};
use kola_utils::{interner::StrInterner, io::FileSystem};

pub enum DriverOptions {}

pub struct Driver {
    io: Arc<dyn FileSystem>,
    arena: Bump,
    str_interner: StrInterner,
    lexicon: BuiltinLexicon,
    print_options: PrintOptions,
}

impl Driver {
    pub fn new(io: impl FileSystem + 'static) -> Self {
        let mut str_interner = StrInterner::default();
        let lexicon = BuiltinLexicon::new(&mut str_interner);

        Self {
            io: Arc::new(io),
            arena: Bump::new(),
            str_interner,
            lexicon,
            print_options: PrintOptions::default(),
        }
    }

    #[inline]
    pub fn parse(
        &mut self,
        path: impl AsRef<Utf8Path>,
    ) -> io::Result<(TreeMap, LocMap, SourceManager)> {
        self._parse(path, true)
    }

    fn _parse(
        &mut self,
        path: impl AsRef<Utf8Path>,
        print: bool,
    ) -> io::Result<(TreeMap, LocMap, SourceManager)> {
        let mut report = Report::new();
        let mut source_manager = SourceManager::new(Arc::clone(&self.io));

        let path = path.as_ref().canonicalize_utf8()?;

        let TokenOutput { token_map, .. } = tokenize(&mut source_manager, &path, &mut report);

        if print {
            for tokens in token_map.values() {
                let printer = TokenPrinter(tokens, self.print_options);
                printer.print(self.print_options, &self.arena);
            }
        }

        let mut tree_map = TreeMap::new();
        let mut loc_map = LocMap::new();

        for (source_id, tokens) in token_map {
            let ParseOutput {
                tokens: _,
                tree,
                spans,
                recovered,
            } = parse(
                ParseInput::new(source_id, tokens, &mut self.str_interner),
                &mut report,
            );

            loc_map.insert(source_id, spans);

            if let Some(tree) = tree {
                if print {
                    let decorators = Decorators::new();
                    let printer = TreePrinter::root(&tree, &self.str_interner, decorators);
                    printer.print(self.print_options, &self.arena);
                }
                tree_map.insert(source_id, tree);
            }

            if !report.is_empty() {
                report.eprint(&source_manager)?;
                break;
            }

            if !recovered.is_empty() {
                eprintln!(
                    "{} issues recovered during parsing.",
                    "Warning:".bold().yellow(),
                );
                recovered.eprint(&source_manager)?;
                break;
            }
        }

        Ok((tree_map, loc_map, source_manager))
    }

    #[inline]
    pub fn analyze(
        &mut self,
        path: impl AsRef<Utf8Path>,
    ) -> io::Result<
        Option<(
            TreeMap,
            LocMap,
            SourceManager,
            ResolveOutput,
            TypeCheckOutput,
        )>,
    > {
        self._analyze(path, true)
    }

    fn _analyze(
        &mut self,
        path: impl AsRef<Utf8Path>,
        print: bool,
    ) -> io::Result<
        Option<(
            TreeMap,
            LocMap,
            SourceManager,
            ResolveOutput,
            TypeCheckOutput,
        )>,
    > {
        let mut report = Report::new();

        let (tree_map, loc_map, source_manager) = self._parse(path.as_ref(), false)?;

        let resolve_output = resolve(
            &tree_map,
            &loc_map,
            &self.arena,
            &mut self.str_interner,
            &mut report,
            self.print_options,
        )?;

        let ResolveOutput {
            modules,
            defs,
            module_graph,
            entry_points,
            value_orders,
            type_orders,
            module_type_orders,
            module_order,
        } = &resolve_output;

        if print {
            // TODO provide a way to filter or select specific modules to print
            for (sym, module) in modules {
                let module_def = defs[*sym];
                let tree = &tree_map[&module_def.loc.path];

                let resolution_decorator = ResolutionDecorator(&module.nodes);
                let decorators = Decorators::new().with(&resolution_decorator);

                let tree_printer =
                    TreePrinter::new(tree, &self.str_interner, decorators, module_def.id);

                println!(
                    "{} SourceId {}, ModuleSym {}\n{}",
                    "Resolved Abstract Syntax Tree".bold().bright_white(),
                    module_def.loc.path,
                    sym,
                    tree_printer.render(self.print_options, &self.arena)
                );
            }

            println!(
                "{} Module Graph:\n{}",
                "Module Graph".bold().bright_white(),
                module_graph.to_dot()
            );
        }

        if !report.is_empty() {
            report.eprint(&source_manager)?;
            return Ok(None);
        }

        let type_check_output = type_check(
            &tree_map,
            &loc_map,
            &modules,
            &defs,
            &entry_points,
            &module_order,
            &type_orders,
            &value_orders,
            &self.arena,
            &mut self.str_interner,
            &self.lexicon,
            &mut report,
            self.print_options,
        );

        let TypeCheckOutput {
            global_env,
            type_annotations,
        } = &type_check_output;

        if print {
            for (sym, module) in modules {
                let module_def = defs[*sym];
                let tree = &tree_map[&module_def.loc.path];

                let Some(annots) = type_annotations.get(sym) else {
                    continue;
                };

                let resolution_decorator = ResolutionDecorator(&module.nodes);
                let type_decorator = TypeDecorator(annots);
                let decorators = Decorators::new()
                    .with(&resolution_decorator)
                    .with(&type_decorator);

                let tree_printer =
                    TreePrinter::new(&tree, &self.str_interner, decorators, module_def.id);

                println!(
                    "{} SourceId {}, ModuleSym {}\n{}",
                    "Typed Abstract Syntax Tree".bold().bright_white(),
                    module_def.loc.path,
                    sym,
                    tree_printer.render(self.print_options, &self.arena)
                );
            }
        }

        if !report.is_empty() {
            report.eprint(&source_manager)?;
            return Ok(None);
        }

        Ok(Some((
            tree_map,
            loc_map,
            source_manager,
            resolve_output,
            type_check_output,
        )))
    }

    pub fn compile(&mut self, path: impl AsRef<Utf8Path>) -> io::Result<Option<Program>> {
        self._compile(path, true)
    }

    fn _compile(&mut self, path: impl AsRef<Utf8Path>, print: bool) -> io::Result<Option<Program>> {
        let mut report = Report::new();

        let Some((
            tree_map,
            loc_map,
            source_manager,
            ResolveOutput {
                modules,
                defs,
                module_graph,
                entry_points,
                value_orders,
                type_orders,
                module_type_orders,
                module_order,
            },
            TypeCheckOutput {
                global_env,
                type_annotations,
            },
        )) = self._analyze(path, false)?
        else {
            return Ok(None);
        };

        // TODO entry points should return locs for better error reporting
        let &[entry_point] = entry_points.as_slice() else {
            report.add_issue(
                Issue::error("No entry point, or multiple entry points defined.", 0)
                    .with_help("Ensure that exactly one entry point is defined."),
            );
            report.eprint(&source_manager)?;
            return Ok(None);
        };

        let program = lower(
            entry_point,
            &modules,
            &defs,
            &module_order,
            &value_orders,
            &tree_map,
            &self.arena,
            &self.str_interner,
            self.print_options,
        );

        let Program { ir, modules } = &program;

        if print {
            println!(
                "{}\n{}",
                "Intermediate Representation".bold().bright_white(),
                render_ir(&ir, &self.arena, &self.str_interner, self.print_options)
            );
        }

        Ok(Some(program))
    }

    pub fn run(mut self, path: impl AsRef<Utf8Path>) -> io::Result<()> {
        let path = path.as_ref().canonicalize_utf8()?;

        let Some(Program { ir, modules }) = self._compile(&path, false)? else {
            return Ok(());
        };

        let context = Ctx::new(ir, self.lexicon, path.parent().unwrap());
        let mut machine = Machine::new(context);

        let mut heap = Heap::new(self.str_interner);

        match machine.run(&mut heap) {
            Ok(value) => {
                println!("\nExecution result: {}", heap.with(&value))
            }
            Err(e) => eprintln!("\nRuntime error: {}", e),
        }

        Ok(())
    }
}

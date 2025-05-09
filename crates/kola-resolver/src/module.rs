use std::{collections::HashMap, fmt, rc::Rc};

use kola_print::PrintOptions;
use kola_syntax::span::Span;
use kola_tree::{
    id::Id,
    node::{self, Vis},
};
use kola_utils::as_variant;
use kola_vfs::{
    diag::SourceReport,
    file::{FileInfo, FileInfoTable, FileParser},
    path::{FilePath, ImportPath},
    source::Source,
};

use crate::bind::{BindInfo, ModuleBind};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId {
    pub id: Id<node::Module>,
    pub file_path: FilePath,
    pub span: Span,
}

impl ModuleId {
    pub fn new(id: Id<node::Module>, file_path: FilePath, span: Span) -> Self {
        Self {
            id,
            file_path,
            span,
        }
    }

    pub fn from_file(file: &FileInfo) -> Self {
        let id = file.tree.root_id();
        let span = file.span(id);

        Self {
            id,
            file_path: file.source.file_path(),
            span,
        }
    }
}

impl fmt::Display for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ModuleId")
            .field("id", &self.id.as_usize())
            .field("path", &self.file_path)
            .field("span", &self.span)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BindKey {
    pub ident: StrKey,
    pub vis: Vis,
}

impl BindKey {
    pub fn new(ident: StrKey, vis: Vis) -> Self {
        Self { ident, vis }
    }
}

pub type ModulePath = Vec<StrKey>;

#[derive(Debug, Clone)]
pub enum ModuleResolution {
    Resolved(ModuleId),
    Unresolved(ModulePath),
}

impl ModuleResolution {
    pub fn is_resolved(&self) -> bool {
        matches!(self, Self::Resolved(_))
    }

    pub fn is_unresolved(&self) -> bool {
        matches!(self, Self::Unresolved(_))
    }

    pub fn as_resolved(&self) -> Option<&ModuleInfo> {
        as_variant!(self, Self::Resolved)
    }

    pub fn as_unresolved(&self) -> Option<&ModulePath> {
        as_variant!(self, Self::Unresolved)
    }
}

pub type ModuleInfo = HashMap<BindKey, ModuleResolution>;

// // builder
// #[derive(Debug, Clone)]
// pub struct ModuleInfo {
//     pub id: Id<node::Module>,
//     pub span: Span,
//     pub file: FileInfo,
//     pub modules: HashMap<BindKey, ModuleResolution>,
// }

// impl ModuleInfo {
//     pub fn new(id: Id<node::Module>, span: Span, file: FileInfo) -> Self {
//         Self {
//             id,
//             span,
//             file,
//             modules: HashMap::new(),
//         }
//     }

//     pub fn add_module(
//         &mut self,
//         symbol: StrKey,
//         bind: ModuleBind,
//         options: PrintOptions,
//     ) -> Result<(), SourceReport> {
//         let key = BindKey::new(symbol, bind.vis);

//         match *bind.id.get(&self.file).value.get(&self.file) {
//             node::ModuleExpr::Import(import_id) => {
//                 let name = import_id.get(&self.file).0.get(&self.file);
//                 // TODO when inside a nested module this will still return Ok
//                 // because it will just use the root file
//                 // It should however return an error
//                 let import_path = self
//                     .file
//                     .try_import_path()
//                     .map_err(|err| self.file.report(import_id, err))?;
//                 let (file_path, import_path) = import_path
//                     .discover(name)
//                     .map_err(|err| self.file.report(import_id, err))?;
//                 let source = Source::from_path(file_path, import_path)
//                     .map_err(|err| self.file.report(import_id, err))?;
//                 let file = FileParser::new(source, options).try_parse()?;
//                 let id = file.tree.root_id();

//                 let info = ModuleInfo::new(id, file.span(id), file);
//                 let resolution = ModuleResolution::Resolved(info);
//                 self.modules.insert(key, resolution);
//             }
//             node::ModuleExpr::Module(id) => {
//                 let info = ModuleInfo::new(id, self.file.span(id), self.file.clone());
//                 let resolution = ModuleResolution::Resolved(info);
//                 self.modules.insert(key, resolution);
//             }
//             node::ModuleExpr::Path(id) => {
//                 let path = id
//                     .get(&self.file)
//                     .0
//                     .iter()
//                     .map(|id| id.get(&self.file).0.clone())
//                     .collect();

//                 let resolution = ModuleResolution::Unresolved(path);
//                 self.modules.insert(key, resolution);
//             }
//         }

//         Ok(())
//     }
// }

pub struct ModuleInfoTable {
    files: FileInfoTable,
    modules: HashMap<ModuleId, ModuleInfo>,
    parents: HashMap<ModuleId, ModuleId>,
    binds: HashMap<ModuleId, BindInfo>,
}

impl ModuleInfoTable {
    pub fn new() -> Self {
        Self {
            files: FileInfoTable::new(),
            modules: HashMap::new(),
            parents: HashMap::new(),
            binds: HashMap::new(),
        }
    }

    pub fn builder(&mut self) -> ModuleFileBuilder<'_> {
        ModuleBuilder { table: self }
    }
}

pub struct ModuleFileBuilder<'a> {
    table: &'a mut ModuleInfoTable,
}

impl<'a> ModuleFileBuilder<'a> {
    pub fn open(file_path: FilePath, import_path: Option<ImportPath>) -> ModuleBindBuilder {}
}

pub struct ModuleBindBuilder<'a> {
    table: &'a mut ModuleInfoTable,
    file: FileInfo,
}

/// Resolves the module path
/// 1. Looks for module of the first segment in the current module
/// 2. For every other segment looks in the corresponding module
///     and checks if a module exists for it and if it is marked as `export`
///
/// Returns None if some segment of the path could not be resolved
pub fn explore_path(path: &ModulePath, info: &ModuleInfo) -> Option<ModulePath> {
    let (id, path) = path_id.get(tree).0.split_first()?;
    let name = id.get(tree);
    let mut module_path = &module.module(name)?.path;

    for id in path {
        let name = id.get(tree);
        let module = module_infos.get(module_path)?.module(name)?;

        if module.vis != Vis::Export {
            return None;
        }
        module_path = &module.path;
    }

    Some(module_path.to_owned())
}

pub fn try_explore_path(
    path_id: Id<node::ModulePath>,
    span: Span,
    tree: &impl TreeView,
    module: &impl ModuleInfoView,
    module_infos: &ModuleInfoTable,
) -> Result<ModulePath, SourceDiagnostic> {
    explore_path(path_id, tree, module, module_infos).ok_or(
        SourceDiagnostic::error(span, "Module not found")
            .with_help("Maybe the module is not marked to be exported."),
    )
}

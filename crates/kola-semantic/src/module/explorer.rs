use std::collections::HashMap;

use kola_syntax::prelude::*;
use kola_tree::{node::Vis, prelude::*};

use super::{ModuleId, ModuleInfo, ModuleInfoView};
use crate::{env::TypeEnv, types::PolyType};

#[derive(Debug, Clone, Hash)]
pub enum PathResolution {
    LocalValue(PolyType),
    LocalRecordAccess {
        pt: PolyType,
        remaining: Vec<Id<node::Name>>,
    },
    ScopeValue(Id<node::ValueBind>),
    ScopeRecordAccess {
        value_id: Id<node::ValueBind>,
        remaining: Vec<Id<node::Name>>,
    },
    ModuleValue {
        module_id: ModuleId,
        value_id: Id<node::ValueBind>,
    },
    ModuleRecordAccess {
        module_id: ModuleId,
        value_id: Id<node::ValueBind>,
        remaining: Vec<Id<node::Name>>,
    }, // External // Reserved for potential external packages
}

impl PathResolution {
    pub const fn local(pt: PolyType) -> Self {
        Self::LocalValue(pt)
    }

    pub fn local_record(pt: PolyType, remaining: &[Id<node::Name>]) -> Self {
        Self::LocalRecordAccess {
            pt,
            remaining: remaining.to_vec(),
        }
    }

    pub const fn scope(id: Id<node::ValueBind>) -> Self {
        Self::ScopeValue(id)
    }

    pub fn scope_record(value_id: Id<node::ValueBind>, remaining: &[Id<node::Name>]) -> Self {
        Self::ScopeRecordAccess {
            value_id,
            remaining: remaining.to_vec(),
        }
    }

    pub const fn module(module_id: ModuleId, value_id: Id<node::ValueBind>) -> Self {
        Self::ModuleValue {
            module_id,
            value_id,
        }
    }

    pub fn module_record(
        module_id: ModuleId,
        value_id: Id<node::ValueBind>,
        remaining: &[Id<node::Name>],
    ) -> Self {
        Self::ModuleRecordAccess {
            module_id,
            value_id,
            remaining: remaining.to_vec(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleExplorer<'a, T, M> {
    pub tree: &'a T,
    pub spans: &'a SpanInfo,
    pub module_id: &'a ModuleId,
    pub module: &'a M,
    pub global: &'a HashMap<ModuleId, ModuleInfo>,
}

impl<'a, T, M> ModuleExplorer<'a, T, M>
where
    T: TreeView,
    M: ModuleInfoView,
{
    pub fn new(
        tree: &'a T,
        spans: &'a SpanInfo,
        module_id: &'a ModuleId,
        module: &'a M,
        global: &'a HashMap<ModuleId, ModuleInfo>,
    ) -> Self {
        Self {
            tree,
            spans,
            module_id,
            module,
            global,
        }
    }

    fn span<U>(&self, id: Id<U>) -> Span
    where
        U: MetaCast<SyntaxPhase, Meta = Span>,
    {
        *self.spans.meta(id)
    }

    /// Resolves path_expr to `PathResolution` if possible.
    pub fn path_expr(
        self,
        path_id: Id<node::PathExpr>,
        scope: &TypeEnv,
    ) -> Result<PathResolution, SourceDiagnostic> {
        let span = self.span(path_id);

        let (mut seg_id, mut path) =
            path_id
                .get(self.tree)
                .0
                .as_slice()
                .split_first()
                .ok_or(SourceDiagnostic::error(
                    span,
                    "Could not resolve empty path",
                ))?;

        let mut seg = seg_id.get(self.tree);

        // First I need to check the local scope because it might shadow top-level module or value-binds.

        if let Some(pt) = scope.lookup(&seg.0) {
            if path.is_empty() {
                return Ok(PathResolution::local(pt.clone()));
            } else {
                return Ok(PathResolution::local_record(pt.clone(), path));
            }
        }

        let module_id =
            // Resolves the first segment to a locally defined module,
            // bypassing visibility checks, if present.
            if let Some(mut module_id) = self.module.module(&seg.0).map(|bind| &bind.id) {
                while let Some((id, remaining)) = path.split_first() {
                    seg = id.get(self.tree);
                    seg_id = id;

                    if let Some(module) = self
                        .global
                        .get(&module_id)
                        .and_then(|info| info.module(&seg.0))
                        && module.vis == Vis::Export
                    {
                        module_id = &module.id;
                        path = remaining;
                    } else {
                        break;
                    }
                }
                module_id
            } else {
                self.module_id
            };

        if module_id == self.module_id {
            // locally defined value bind
            if let &[id] = path {
                let s_span = self.span(id);
                let s = id.get(self.tree);
                let value = self.module.value(&s.0).ok_or(
                    SourceDiagnostic::error(s_span, "Value binding not found in the current scope")
                        .with_trace([("In this expression".to_owned(), span)]),
                )?;

                Ok(PathResolution::scope(value.id))
            } else if let Some((id, remaining)) = path.split_first() {
                let s_span = self.span(*id);
                let s = id.get(self.tree);
                let value = self.module.value(&s.0).ok_or(
                    SourceDiagnostic::error(
                        s_span,
                        "Record value binding not found in the current scope",
                    )
                    .with_trace([("In this expression".to_owned(), span)]),
                )?;

                Ok(PathResolution::scope_record(value.id, remaining))
            } else {
                // This would mean that the user was able to refer to the module itsself
                // and only the module itsself.
                // This is syntactically not expressable.
                unreachable!()
            }
        } else if module_id.is_parent_of(self.module_id) {
            let module = &self.global[module_id];

            // direct parent of this module
            // access to private value binds is allowed here
            if let &[id] = path {
                let s_span = self.span(id);
                let s = id.get(self.tree);
                let value = module.value(&s.0).ok_or(
                    SourceDiagnostic::error(s_span, "Value binding not found in the parent scope")
                        .with_trace([("In this expression".to_owned(), span)]),
                )?;

                Ok(PathResolution::module(module_id.clone(), value.id))
            } else if let Some((id, remaining)) = path.split_first() {
                let s_span = self.span(*id);
                let s = id.get(self.tree);
                let value = module.value(&s.0).ok_or(
                    SourceDiagnostic::error(
                        s_span,
                        "Record value binding not found in the parent scope",
                    )
                    .with_trace([("In this expression".to_owned(), span)]),
                )?;

                Ok(PathResolution::module_record(
                    module_id.clone(),
                    value.id,
                    remaining,
                ))
            } else {
                // The user just specified the "super" keyword in a expression context
                Err(SourceDiagnostic::error(
                    span,
                    "The 'super' keyword  is not allowed in this context",
                )
                .with_help("Did you want to refer to a member of the parent module?")
                .with_trace([("In this expression".to_owned(), span)]))
            }
        } else {
            let module = &self.global[module_id];

            // sibling or cousing modules, need to enforce visibilty here
            if let &[id] = path {
                let s_span = self.span(id);
                let s = id.get(self.tree);
                let value = module.value(&s.0).ok_or(
                    SourceDiagnostic::error(s_span, "Value binding not found.").with_trace([
                        ("In this scope".to_owned(), self.span(*seg_id)),
                        ("In this expression".to_owned(), span),
                    ]),
                )?;

                if value.vis != Vis::Export {
                    return Err(SourceDiagnostic::error(
                        s_span,
                        format!("Value binding was found but it is not exported."),
                    )
                    .with_help("You might want to export it using the 'export' keyword.")
                    .with_trace([
                        ("In this scope".to_owned(), self.span(*seg_id)),
                        ("In this expression".to_owned(), span),
                    ]));
                }

                Ok(PathResolution::module(module_id.clone(), value.id))
            } else if let Some((id, remaining)) = path.split_first() {
                let s_span = self.span(*id);
                let s = id.get(self.tree);
                let value = module.value(&s.0).ok_or(
                    SourceDiagnostic::error(s_span, "Record value binding not found.").with_trace(
                        [
                            ("In this scope".to_owned(), self.span(*seg_id)),
                            ("In this expression".to_owned(), span),
                        ],
                    ),
                )?;

                if value.vis != Vis::Export {
                    return Err(SourceDiagnostic::error(
                        s_span,
                        format!("Record value binding was found but it is not exported."),
                    )
                    .with_help("You might want to export it using the 'export' keyword.")
                    .with_trace([
                        ("In this scope".to_owned(), self.span(*seg_id)),
                        ("In this expression".to_owned(), span),
                    ]));
                }

                Ok(PathResolution::module_record(
                    module_id.clone(),
                    value.id,
                    remaining,
                ))
            } else {
                Err(SourceDiagnostic::error(
                    span,
                    "Refering to just a module is not allowed in this context",
                )
                .with_help("Did you want to refer to a member of the specified module?")
                .with_trace([("In this scope".to_owned(), self.span(*seg_id))]))
            }
        }
    }

    /// Resolves the module path
    /// 1. Looks for module of the first segment in the current module
    /// 2. For every other segment looks in the corresponding module
    ///     and checks if a module exists for it and if it is marked as `export`
    ///
    /// Returns None if some segment of the path could not be resolved
    pub fn module_path(self, path_id: Id<node::ModulePath>) -> Option<ModuleId> {
        // TODO move error reporting here
        let path = path_id.get(self.tree);
        let mut segments = path.0.iter();

        let first_id = segments.next()?;
        let first = first_id.get(self.tree);
        let mut module_id = self.module.module(&first.0)?.id.clone();

        for id in segments {
            let s = id.get(self.tree);
            let module = self.global.get(&module_id)?.module(&s.0)?;

            if module.vis != Vis::Export {
                return None;
            }
            module_id = module.id.clone();
        }

        Some(module_id)
    }
}

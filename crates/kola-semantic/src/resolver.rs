use grax::{
    core::{
        collections::{
            FixedNodeMap, GetNode, GetNodeMut, IndexNode, InsertEdge, InsertNode, NodeCount,
            NodeIter, NodeMap, VisitNodeMap,
        },
        graph::{Create, EdgeIterAdjacent, NodeAttribute, NodeIterAdjacent},
    },
    prelude::*,
};
use kola_utils::as_variant;
use miette::Diagnostic;
use std::{
    collections::{HashMap, VecDeque},
    io,
    ops::ControlFlow,
};
use thiserror::Error;

use kola_print::PrintOptions;
use kola_syntax::prelude::*;
use kola_tree::{
    node::{Symbol, Vis},
    prelude::*,
};
use kola_vfs::prelude::*;

use crate::{
    VisitState,
    module::{
        ModuleBind, ModuleInfo, ModuleInfoBuilder, ModuleInfoTable, ModulePath, TypeBind, ValueBind,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleLink {
    pub vis: Vis,
    pub specifier: Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleTreeNode {
    Resolved(ModulePath),
    Unresolved(Vec<Symbol>),
}

impl ModuleTreeNode {
    pub fn is_resolved(&self) -> bool {
        matches!(self, Self::Resolved(_))
    }

    pub fn is_unresolved(&self) -> bool {
        matches!(self, Self::Unresolved(_))
    }

    pub fn as_resolved(&self) -> Option<&ModulePath> {
        as_variant!(self, Self::Resolved)
    }

    pub fn as_unresolved(&self) -> Option<&Vec<Symbol>> {
        as_variant!(self, Self::Unresolved)
    }
}

impl ModuleTreeNode {
    pub fn from_bind(
        file_infos: &mut FileInfoTable,
        bind: &ModuleBind,
        file: &FileInfo,
        options: PrintOptions,
    ) -> Result<Self, SourceReport> {
        match bind.id.get(&file.tree).value.get(&file.tree) {
            node::ModuleExpr::Import(id) => {
                let name = id.get(&file.tree).0.get(&file.tree);
                // TODO when inside a nested module this will still return Ok
                // because it will just use the root file
                // It should however return an error
                let import_path = file
                    .try_import_path()
                    .map_err(|err| file.report(*id, err))?;
                let (file_path, import_path) = import_path
                    .discover(name)
                    .map_err(|err| file.report(*id, err))?;
                let source = Source::from_path(file_path, import_path)
                    .map_err(|err| file.report(*id, err))?;
                let file = FileParser::new(source, options).try_parse()?;
                file_infos.insert(file.source.file_path(), file.clone());

                let path = ModulePath::from_file(&file);
                Ok(Self::Resolved(path))
            }
            node::ModuleExpr::Module(id) => {
                let span = *file.spans.meta(*id);
                let path = ModulePath::new(*id, file.source.file_path(), Some(span));
                Ok(Self::Resolved(path))
            }
            node::ModuleExpr::Path(id) => {
                let path = id
                    .get(&file.tree)
                    .0
                    .iter()
                    .map(|id| id.get(&file.tree).0.clone())
                    .collect();

                Ok(Self::Unresolved(path))
            }
        }
    }
}

pub type ModuleId = grax::core::index::NodeId<usize>;
pub type ModuleTree = AdjGraph<ModuleTreeNode, ModuleLink, true>;

#[derive(Error, Debug, Diagnostic)]
#[error("Module Report:")]
pub struct ModuleReport {
    #[label = "In this module"]
    pub span: Option<Span>,
    #[source_code]
    pub src: Source,
    #[related]
    pub related: Vec<SourceDiagnostic>,
}

impl ModuleReport {
    pub fn new(report: SourceReport, span: Option<Span>) -> Self {
        Self {
            span,
            src: report.src,
            related: report.related,
        }
    }
}

impl From<SourceReport> for ModuleReport {
    fn from(report: SourceReport) -> Self {
        Self {
            span: None,
            src: report.src,
            related: report.related,
        }
    }
}

#[derive(Error, Debug, Default, Diagnostic)]
#[error("Module Reports:")]
pub struct ModuleReports(#[related] Vec<ModuleReport>);

pub struct ModuleTreeReport {
    tree: ModuleTree,
    root_id: Option<ModuleId>,
    parents: HashMap<ModuleId, ModuleId>,
    file_infos: FileInfoTable,
    module_infos: ModuleInfoTable,
    reports: ModuleReports,
}

pub fn discover(
    file_path: FilePath,
    import_path: Option<ImportPath>,
    options: PrintOptions,
) -> io::Result<ModuleTreeReport> {
    let mut reports = Vec::new();
    let mut file_infos = FileInfoTable::new();
    let mut module_infos = ModuleInfoTable::new();
    let mut tree = ModuleTree::new();
    let mut parents = HashMap::new();
    let mut stack = Vec::new();

    let source = Source::from_path(file_path.clone(), import_path)?;
    let file = match FileParser::new(source, options).try_parse() {
        Ok(file) => file,
        Err(report) => {
            reports.push(report.into());
            return Ok(ModuleTreeReport {
                tree,
                root_id: None,
                parents,
                file_infos,
                module_infos,
                reports: ModuleReports(reports),
            });
        }
    };
    let path = ModulePath::from_file(&file);
    let root_id = tree.insert_node(ModuleTreeNode::Resolved(path));

    stack.push(root_id);
    file_infos.insert(file_path, file);

    while let Some(from) = stack.pop() {
        let path = tree
            .node(from)
            .unwrap()
            .weight
            .as_resolved()
            .unwrap()
            .to_owned();

        let file = file_infos[&path.file_path].clone();
        let mut discoverer = ModuleDiscoverer::new(file.clone());

        path.id.visit_by(&mut discoverer, &file.tree);

        let info = match discoverer.finish() {
            Ok(info) => info,
            Err(report) => {
                reports.push(report.into());
                continue;
            }
        };

        for (symbol, bind) in info.modules.iter() {
            let node = match ModuleTreeNode::from_bind(&mut file_infos, bind, &file, options) {
                Ok(node) => node,
                Err(report) => {
                    reports.push(report.into());
                    continue;
                }
            };

            let is_resolved = node.is_resolved();
            let to = tree.insert_node(node);

            if is_resolved {
                stack.push(to);
            }

            // if !is_resolved && bind.vis == Vis::Export {
            //     panic!("Module aliases/path are not allowed to be export")
            // }

            tree.insert_edge(
                from,
                to,
                ModuleLink {
                    vis: bind.vis,
                    specifier: symbol.to_owned(),
                },
            );
            parents.insert(to, from);
        }

        module_infos.insert(path, info);
    }

    Ok(ModuleTreeReport {
        tree,
        root_id: Some(root_id),
        parents,
        file_infos,
        module_infos,
        reports: ModuleReports(reports),
    })
}

struct ModuleDiscoverer {
    builder: ModuleInfoBuilder,
    errors: Vec<SourceDiagnostic>,
    file: FileInfo,
}

impl ModuleDiscoverer {
    fn new(file: FileInfo) -> Self {
        Self {
            builder: ModuleInfoBuilder::new(),
            errors: Vec::new(),
            file,
        }
    }

    #[inline]
    fn span<T>(&self, id: Id<T>) -> Span
    where
        T: MetaCast<SyntaxPhase, Meta = Span>,
    {
        self.file.span(id)
    }

    #[inline]
    fn report(&mut self, diag: impl Into<SourceDiagnostic>) {
        self.errors.push(diag.into())
    }

    fn finish(self) -> Result<ModuleInfo, SourceReport> {
        let Self {
            builder,
            errors,
            file,
        } = self;

        if errors.is_empty() {
            Ok(builder.finish())
        } else {
            let report = SourceReport::new(file.source.clone(), errors);
            Err(report)
        }
    }
}

impl<T> Visitor<T> for ModuleDiscoverer
where
    T: TreeView,
{
    type BreakValue = ();

    // This will only traverse the current module's scope,
    // because the different module branches are not visited any further under `visit_module_bind`
    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let node::ValueBind { vis, name, .. } = *id.get(tree);

        let vis = *vis.get(tree);
        let name = name.get(tree).0.clone();

        if let Err(diag) = self
            .builder
            .insert_value(name, ValueBind::new(id, vis, span))
        {
            self.report(diag);
        }

        ControlFlow::Continue(())
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let node::TypeBind { name, .. } = *id.get(tree);

        let name = name.get(tree).0.clone();

        if let Err(diag) = self.builder.insert_type(name, TypeBind::new(id, span)) {
            self.report(diag);
        }
        ControlFlow::Continue(())
    }

    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let node::ModuleBind { vis, name, .. } = *id.get(tree);

        let vis = *vis.get(tree);
        let name = name.get(tree).0.clone();

        if let Err(diag) = self
            .builder
            .insert_module(name, ModuleBind::new(id, vis, span))
        {
            self.report(diag);
        }
        ControlFlow::Continue(())
    }
}

/// Restrictions:
///
/// - inline modules cannot contain imports
/// - module paths (aliases) cannot be export
///     - or maybe better: aliases can only be final component of another alias
pub struct ModuleGraph(AdjGraph<ModulePath, ModuleLink, true>);

impl ModuleGraph {
    pub fn explore(tree: ModuleTree, parents: HashMap<ModuleId, ModuleId>) -> Self {
        // let node_ids = tree
        //     .iter_nodes()
        //     .filter_map(|NodeRef { node_id, weight }| {
        //         if weight.is_unresolved() {
        //             Some(node_id)
        //         } else {
        //             None
        //         }
        //     })
        //     .collect::<Vec<_>>();

        // let mut links = node_ids
        //     .iter()
        //     .map(|id| {
        //         (
        //             *id,
        //             resolve_path(
        //                 parents[id],
        //                 tree[*id].as_unresolved().unwrap(),
        //                 &parents,
        //                 &tree,
        //             ),
        //         )
        //     })
        //     .collect::<HashMap<_, _>>();

        // let mut updated = false;

        // for _ in 1..node_ids.len() {}

        // let mut links = HashMap::new();

        // while let Some(id) = stack.pop() {
        //     if let ModuleTreeNode::Unresolved(path) = &tree[id] {
        //         match resolve_path(id, path, parents, &tree) {
        //             Some(target_id) => {
        //                 links.insert(id, target_id);
        //             }
        //             None => stack.push(id),
        //         }
        //     }
        // }

        // let mut order = Vec::new();
        // let mut visited = tree.fixed_node_map(VisitState::Unvisited);
        // while let Some(id) = stack.pop() {
        //     match visited[id] {
        //         VisitState::Unvisited => {
        //             visited.update_node(id, VisitState::Visiting);
        //             stack.push(id);
        //         }
        //         VisitState::Visiting => {
        //             visited.update_node(id, VisitState::Visited);
        //             order.push(id);
        //             continue;
        //         }
        //         VisitState::Visited => continue,
        //     }

        //     let weight = tree.node(id).unwrap().weight;

        //     if let ModuleTreeNode::Unresolved(path) = weight {
        //         let mut path = path.as_slice();
        //         let mut target_id = parents[&id];

        //         while let Some((specifier, remaining)) = path.split_first() {
        //             if let Some(next_id) = tree.iter_adjacent_edges(target_id).find_map(
        //                 |EdgeRef { edge_id, weight }| {
        //                     if &weight.specifier == specifier && weight.vis == Vis::Export {
        //                         Some(edge_id.to())
        //                     } else {
        //                         None
        //                     }
        //                 },
        //             ) {
        //                 target_id = next_id;
        //                 path = remaining;
        //             } else {
        //                 break;
        //             }
        //         }

        //         let target_weight = tree.node(target_id).unwrap().weight;

        //         if target_weight.is_unresolved() {
        //             stack.push(target_id);
        //         } else if path.is_empty() {
        //             links.insert(id, target_id);
        //         } else {
        //             panic!()
        //         }
        //     }
        // }

        // let mut stack = tree
        //     .iter_nodes()
        //     .filter_map(|NodeRef { node_id, weight }| {
        //         if let Some(path) = weight.as_unresolved() {
        //             if let Some((first, path)) = path.split_first()
        //                 && first == "super"
        //             {
        //                 Some((node_id, parents[&parents[&node_id]], path))
        //             } else {
        //                 Some((node_id, parents[&node_id], path))
        //             }
        //         } else {
        //             None
        //         }
        //     })
        //     .collect::<Vec<_>>();

        // while let Some((id, mut target_id, mut path)) = stack.pop() {
        //     match visited[id] {
        //         VisitState::Unvisited => {
        //             visited.update_node(id, VisitState::Visiting);
        //             stack.push((id, target_id, path));
        //         }
        //         VisitState::Visiting => {
        //             visited.update_node(id, VisitState::Visited);
        //             order.push(id);
        //             continue;
        //         }
        //         VisitState::Visited => continue,
        //     }

        //     while let Some((specifier, remaining)) = path.split_first() {
        //         if let Some(next_id) =
        //             tree.iter_adjacent_edges(target_id)
        //                 .find_map(|EdgeRef { edge_id, weight }| {
        //                     if &weight.specifier == specifier {
        //                         Some(edge_id.to())
        //                     } else {
        //                         None
        //                     }
        //                 })
        //         {
        //             target_id = next_id;
        //             path = remaining;
        //         } else {
        //             break;
        //         }
        //     }

        //     if path.is_empty() {
        //         links.insert(id, target_id);
        //     } else {
        //         stack.push((id, target_id, path));
        //     }
        // }

        todo!()
    }
}

fn just_resolve(
    mut target_id: ModuleId,
    mut path: &[Symbol],
    parents: &HashMap<ModuleId, ModuleId>,
    tree: &ModuleTree,
) -> Option<ModuleId> {
    while let Some((specifier, remaining)) = path.split_first() {
        target_id = if specifier == "super" {
            parents[&target_id]
        } else {
            tree.iter_adjacent_edges(target_id)
                .find_map(|EdgeRef { edge_id, weight }| {
                    if &weight.specifier == specifier {
                        if weight.vis != Vis::Export {
                            // TODO create error
                            panic!("whoopsis please mark as export");
                        }

                        Some(edge_id.to())
                    } else {
                        None
                    }
                })?
        };

        path = remaining;
    }

    Some(target_id)
}

fn resolve_path(
    id: ModuleId,
    mut path: &[Symbol],
    visited: &mut <ModuleTree as NodeAttribute>::FixedNodeMap<VisitState>,
    parents: &HashMap<ModuleId, ModuleId>,
    tree: &mut ModuleTree,
) -> Result<(), String> {
    match visited[id] {
        VisitState::Unvisited => {
            visited[id] = VisitState::Visiting;

            let mut target_id = parents[&id];

            while let Some((specifier, remaining)) = path.split_first() {
                target_id = if specifier == "super" {
                    parents[&target_id]
                } else {
                    match tree.iter_adjacent_edges(target_id).find_map(
                        |EdgeRef { edge_id, weight }| {
                            if &weight.specifier == specifier {
                                if weight.vis != Vis::Export {
                                    // TODO create error
                                    panic!("whoopsis please mark as export");
                                }

                                Some(edge_id.to())
                            } else {
                                None
                            }
                        },
                    ) {
                        Some(id) => id,
                        None => break,
                    }
                };

                path = remaining;
            }

            match &tree[target_id] {
                ModuleTreeNode::Unresolved(target_path) => {
                    resolve_path(target_id, &target_path.to_owned(), visited, parents, tree)?;

                    let finish_id = if path.is_empty() {
                        target_id
                    } else {
                        // hmm this doesnt really work because I might need to resolve in here too
                        just_resolve(target_id, path, parents, tree)
                            .ok_or("not found stuff".to_owned())?
                    };
                    tree.update_node(id, tree[finish_id].clone());

                    Ok(())
                }
                ModuleTreeNode::Resolved(module_path) => {
                    if path.is_empty() {
                        visited[id] = VisitState::Visited;
                        tree.update_node(id, ModuleTreeNode::Resolved(module_path.to_owned()));
                        Ok(())
                    } else {
                        Err("not found stuff".to_owned())
                    }
                }
            }
        }
        VisitState::Visiting => Err("damnn cycle detected bro".to_owned()),
        VisitState::Visited => Ok(()),
    }
}

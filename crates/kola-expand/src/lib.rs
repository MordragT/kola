// use std::collections::HashMap;

// use kola_span::{Report, SourceCache};
// use kola_syntax::loc::LocInfo;
// use kola_tree::{
//     fold::Folder,
//     id::Id,
//     node,
//     tree::{Tree, TreeView},
// };
// use kola_utils::{PathKey, StrKey};

// pub struct ExpandInput {
//     pub source_cache: SourceCache,
//     pub tree: Tree,
//     pub spans: LocInfo,
//     pub report: Report,
// }

// pub type BindVec<T> = Vec<(StrKey, Id<T>)>;

// pub struct Expanded {
//     pub tree: Tree,
//     pub spans: LocInfo,
//     pub modules: BindVec<node::ModuleBind>,
//     pub values: BindVec<node::ValueBind>,
//     pub types: BindVec<node::TypeBind>,
// }

// pub struct ExpandOutput {
//     pub source_cache: SourceCache,
//     pub expansion: HashMap<PathKey, Expanded>,
//     pub report: Report,
// }

// pub struct Expander {}

// impl<T: TreeView> Folder<T> for Expander {
//     fn fold_module_bind(&mut self, id: Id<node::ModuleBind>, tree: &mut T) {
//         let node::ModuleBind {
//             vis,
//             name,
//             ty,
//             value,
//         } = id.get(tree);

//     }
// }

// // State transitions are essentially:
// // 1. Parse
// // 2. Discover module binds and parse imports // TODO into own Tree's or reuse the current one
// //

// // TODO implement Fold for the Tree inside kola-tree
// // then in this expander fold all imports just to Modules
// // then a map from module_ids to PathKey's to still now what is inline and what not
// // inlines can still have imports if there is then a nested dir's

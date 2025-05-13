//! Examples for state-separated module manager with phantom types
//!
//! This module demonstrates how to use the state-separated module manager
//! with phantom-typed module keys.

use kola_span::Loc;
use kola_syntax::loc::Locations;
use kola_tree::{id::Id, node::Vis, tree::Tree};

use crate::state_separated::{
    AnalyzedModuleKey, BaseModuleKey, InlineModuleData, ParsedModuleKey,
    StateSeparatedModuleManager, TypeData, ValueData,
};

/// Example showing the progression of a module through states
pub fn state_progression_example() {
    // Create a new manager
    let mut manager = StateSeparatedModuleManager::new();

    // Register a module (Base state)
    let base_key = manager.register_file_module("test.kl".into(), "test".into());

    // At this point, we can only access base data
    let name = manager.get_name(base_key).unwrap();
    println!("Module name: {}", name);

    // Create dummy tree and spans for parsing
    let dummy_tree = Tree::new();
    let dummy_spans = Locations::default();

    // Parse the module (transition from Base to Parsed state)
    let parsed_key = manager
        .parse_module(base_key, dummy_tree, dummy_spans)
        .unwrap();

    // At this point, we can access both base and parsed data
    let name = manager.get_name(parsed_key).unwrap();
    let tree = manager.get_tree(parsed_key).unwrap();
    println!(
        "Module name: {}, has tree: {}",
        name,
        tree.nodes().len() > 0
    );

    // Try to access parsed data using a base key (would cause type error):
    // let tree = manager.get_tree(base_key); // This won't compile!

    // Analyze the module (transition from Parsed to Analyzed state)
    let analyzed_key = manager.analyze_module(parsed_key).unwrap();

    // At this point, we can access base, parsed, and analyzed data
    let name = manager.get_name(analyzed_key).unwrap();
    let tree = manager.get_tree(analyzed_key).unwrap();

    // Add a value to the analyzed module
    let value_data = ValueData {
        vis: Vis::Export,
        node_id: unsafe { Id::new_unchecked(1) },
        location: Loc::new(1, 1),
    };

    manager
        .insert_value(analyzed_key, "x".into(), value_data)
        .unwrap();

    // Now we can query the value
    let x_value = manager.get_value(analyzed_key, &"x".into()).unwrap();
    println!("Found value x: {}", x_value.is_some());

    // Check if the value is exported
    let is_exported = manager.is_exported(analyzed_key, &"x".into()).unwrap();
    println!("x is exported: {}", is_exported);

    // Try to add a value using a parsed key (would cause type error):
    // manager.insert_value(parsed_key, "y".into(), value_data); // This won't compile!
}

/// Example showing the relationship between different module keys
pub fn key_relationship_example() {
    let mut manager = StateSeparatedModuleManager::new();

    // Register modules representing different phases of a compiler pipeline
    let parse_key = manager.register_file_module("parse.kl".into(), "parse".into());
    let typecheck_key = manager.register_file_module("typecheck.kl".into(), "typecheck".into());
    let codegen_key = manager.register_file_module("codegen.kl".into(), "codegen".into());

    // Add dependencies (parse -> typecheck -> codegen)
    manager.add_dependency(typecheck_key, parse_key);
    manager.add_dependency(codegen_key, typecheck_key);

    // Process the modules
    let dummy_tree = Tree::new();
    let dummy_spans = Locations::default();

    // Parse and analyze each module
    let parse_parsed = manager
        .parse_module(parse_key, dummy_tree.clone(), dummy_spans.clone())
        .unwrap();
    let typecheck_parsed = manager
        .parse_module(typecheck_key, dummy_tree.clone(), dummy_spans.clone())
        .unwrap();
    let codegen_parsed = manager
        .parse_module(codegen_key, dummy_tree.clone(), dummy_spans.clone())
        .unwrap();

    let parse_analyzed = manager.analyze_module(parse_parsed).unwrap();
    let typecheck_analyzed = manager.analyze_module(typecheck_parsed).unwrap();
    let codegen_analyzed = manager.analyze_module(codegen_parsed).unwrap();

    // Get sorted modules
    let sorted = manager.sorted_modules().unwrap();
    println!("Topological order: {:?}", sorted);

    // Each key identifies the same module, but with different state information
    println!("Raw IDs:");
    println!("  parse_key: {}", parse_key.raw_id());
    println!("  parse_analyzed: {}", parse_analyzed.raw_id());
    // They have the same ID, but different types that enforce proper access
}

/// Example showing how to process multiple modules
pub fn multi_module_example() {
    let mut manager = StateSeparatedModuleManager::new();

    // Register several modules
    let keys = vec![
        manager.register_file_module("module1.kl".into(), "module1".into()),
        manager.register_file_module("module2.kl".into(), "module2".into()),
        manager.register_file_module("module3.kl".into(), "module3".into()),
    ];

    let dummy_tree = Tree::new();
    let dummy_spans = Locations::default();

    // Parse all modules (mapping BaseModuleKey to ParsedModuleKey)
    let parsed_keys: Vec<ParsedModuleKey> = keys
        .into_iter()
        .map(|key| {
            manager
                .parse_module(key, dummy_tree.clone(), dummy_spans.clone())
                .unwrap()
        })
        .collect();

    // Analyze all modules (mapping ParsedModuleKey to AnalyzedModuleKey)
    let analyzed_keys: Vec<AnalyzedModuleKey> = parsed_keys
        .into_iter()
        .map(|key| manager.analyze_module(key).unwrap())
        .collect();

    // Now we can work with all the analyzed modules
    for key in analyzed_keys {
        let name = manager.get_name(key).unwrap();
        println!("Analyzed module: {}", name);

        // Add a dummy value to each module
        let value_data = ValueData {
            vis: Vis::Export,
            node_id: unsafe { Id::new_unchecked(1) },
            location: Loc::new(1, 1),
        };

        manager
            .insert_value(key, "dummy".into(), value_data)
            .unwrap();
    }
}

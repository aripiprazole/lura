#[allow(clippy::all)]
#[allow(unused_variables)]
#[allow(dead_code)]
#[allow(non_snake_case)]
#[allow(unused_macros)]
#[allow(unused_parens)]
#[allow(unused_labels)]
#[allow(non_upper_case_globals)]
pub mod generated {
    pub mod lura;
}

pub use generated::lura::*;

#[cfg(test)]
mod tests {
    use tree_sitter::Parser;
    use type_sitter_lib::NodeResultExtraOrExt;

    use crate::SourceFile;

    #[test]
    fn test() {
        let mut parser = Parser::new();
        parser
            .set_language(tree_sitter_lura::language())
            .expect("Error loading lura language");

        let tree = parser
            .parse("Main { IO.println \"Hello, world\" }", None)
            .unwrap();

        let source_file = SourceFile::try_from(tree.root_node()).unwrap();
        for decl in source_file.decls(&mut tree.walk()) {
            let decl = decl.unwrap().unwrap();
            decl.data_decl()
                .unwrap()
                .clause_types(&mut tree.walk())
                .next()
                .unwrap()
                .unwrap2()
                .f_32();

            println!("{decl:?}");
        }
    }
}

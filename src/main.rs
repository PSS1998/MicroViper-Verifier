use miette::Result;
use miette::Report;
pub use syntax::{self, ast};

fn main() -> Result<()> {
    // Parsing example
    for p in std::env::args().skip(1) {
        let doc_ast = syntax::parse_file(p.clone())?;
        // println!("{doc_ast:#?}");

        let new_doc_ast = syntax::encode3(doc_ast.clone())?;
        // println!("{new_doc_ast:#?}");

        let new_new_doc_ast = syntax::encode2(new_doc_ast.clone())?;
        // println!("{new_new_doc_ast:#?}");
        
        let new_new_new_doc_ast = syntax::encode1(new_new_doc_ast.clone())?;
        // println!("{new_new_new_doc_ast:#?}");

        let new_new_new_new_doc_ast = syntax::encode0(new_new_new_doc_ast.clone())?;
        // println!("{new_new_new_new_doc_ast:#?}");

        let src = std::fs::read_to_string(std::path::Path::new(&p));
        let src_content = match src {
            Ok(content) => content,
            Err(_e) => {
                // Handle the error in some way specific to your application
                return Err(Report::msg("Diagnostic-compatible error"));
            }
        };

        let _new_new_new_new_new_doc_ast = syntax::encode2z3(new_new_new_new_doc_ast.clone(), &src_content)?;
    }

    Ok(())
}

use crate::ast::{
    // IF ELSE dependences
    Body, Document, DocumentItem, Expr, Specification, Statement,ExprKind,UOp,Var
};
use std::collections::HashMap;

/// 
#[derive(Debug)]
struct Encode0Context {
}

pub(crate) fn encode(doc: &Document) -> miette::Result<Document> {
    Encode0Context::encode(doc)
}

impl Encode0Context {
    fn encode(doc: & Document) -> miette::Result<Document> {
        let mut new_doc = doc.clone(); // Create a mutable copy of the document

        // helper functions calls

        Ok(new_doc)
    }


    // initializes all variables declared at beginning of method to version 0
    fn freshAssign(doc: &mut Document) -> miette::Result<Document> {
        
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                if let Some(body) = &mut method.body {
                    if let mut statements =  &mut body.statements {
                        body.statements = getFreshStatements(statements)
                    }
                   
                    // in case of Choice
                }
            }
        }
        Ok(doc.clone())
    }


    fn getFreshStatements(statements: &mut Vet<Statement>) -> Vet<Statement> {
    let mut vars_hashmap: HashMap<String, i32> = HashMap::new();
    let mut fresh_statements = Vec::new();

    for statement in statements.iter() {
        match statement {
            // declaration
            Statement::Var(var, _) => {
                // Process variable declaration
                if !vars_hashmap.contains_key(&var.name) {
                    // Insert the variable into the hashmap with an initial count of 0.
                    vars_hashmap.insert(var.name.clone(), 0);
                    let new_var_name = format!("{}0", var.name);
                    fresh_statements.push(Statement::Var(Var { name: new_var_name, ty: var.ty }, None));
            }

            // all those must be on last version of variable
            Statement::Assume
            Statement::Assert
            Statement::Choice
            
            //assignment
            Statement::Assignment(ident, expr) => {
                if let Some(count) = vars_hashmap.get_mut(&ident) {
                    // Increment the counter for the variable.
                    *count += 1;

                    // Generate a new variable name with the count.
                    let new_var = format!("{}{}", ident, count-1);

                    // Push new declaration

                    // Replace the assignment with the new variable name.
                    fresh_statements.push(Statement::Assignment(new_var, expr.clone()));
                }
            }
            // Handle other statement variants here.
            _ => {
                fresh_statements.push(statement.clone());
            }
        }
    }

    fresh_statements
        


    }




}

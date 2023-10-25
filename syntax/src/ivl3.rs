use crate::ast::{
    // IF ELSE dependences
    Body, Document, DocumentItem, Expr, Statement, ExprKind, UOp, Var, Type, Method
};
use std::cmp::Ord;

/// 
#[derive(Debug)]
struct Encode3Context {
}

pub(crate) fn encode(doc: &Document) -> miette::Result<Document> {
    Encode3Context::encode(doc)
}

impl Encode3Context {
    fn encode(doc: & Document) -> miette::Result<Document> {
        let mut new_doc = doc.clone(); // Create a mutable copy of the document
        new_doc = Self::replace_while(&mut new_doc)?;

        Ok(new_doc)
    }

    fn havoc_variable(var: &Var) -> (Statement, Var) {
        let var_declaration = Statement::Var(var.clone(), None);
        (var_declaration, var.clone())
    }

    fn get_written_variables(body: &Body, methods: &mut [Method]) -> Vec<Var> {
        let mut written_vars = Vec::new();
    
        for stmt in &body.statements {
            match stmt {
                Statement::Assignment(ident, expr) => {
                    written_vars.push(Var{name: ident.clone(), ty: expr.ty});
                }
                Statement::MethodAssignment(idents, method_name, _) => {
                    if let Some(index) = methods.iter().position(|m| m.name.text == method_name.text) {
                        let method = &mut methods[index];
                        for i in 0..idents.len() {
                            written_vars.push(Var{name: idents[i].clone(), ty: method.outputs[index].ty});
                        }
                    }
                }
                Statement::If(_, then_body, Some(else_body)) => {
                    written_vars.extend(Self::get_written_variables(then_body, methods));
                    written_vars.extend(Self::get_written_variables(else_body, methods));
                }
                Statement::While { body: while_body, .. } => {
                    written_vars.extend(Self::get_written_variables(while_body, methods));
                }
                Statement::Choice(body1, body2) => {
                    written_vars.extend(Self::get_written_variables(body1, methods));
                    written_vars.extend(Self::get_written_variables(body2, methods));
                }
                _ => {}
            }
        }
    
        // Remove duplicates
        written_vars.sort_by(|a, b| a.name.text.cmp(&b.name.text));
        written_vars.dedup_by(|a, b| a.name.text == b.name.text);
    
        written_vars
    }
    
    fn replace_while_recursive(stmt: &mut Statement, methods: &mut [Method]) {
        match stmt {
            Statement::While { condition, invariants, body } => {
                let condition = condition.clone();
                let mut invariants = invariants.clone();
                let mut new_body = body.clone();

                let written_vars = Self::get_written_variables(body, methods); 
    
                let mut transformed_statements = Vec::new();
    
                for invariant in &mut *invariants {
                    transformed_statements.push(Statement::Assert(invariant.clone()));
                }
    
                for var in &written_vars {
                    let (havoc_stmt, _havoc_var) = Self::havoc_variable(var);
                    transformed_statements.push(havoc_stmt);
                }
    
                for invariant in &mut *invariants {
                    transformed_statements.push(Statement::Assume(invariant.clone()));
                }
    
                let mut body1_statements = vec![Statement::Assume(condition.clone())];
                body1_statements.extend(body.statements.clone());
                for invariant in &invariants {
                    body1_statements.push(Statement::Assert(invariant.clone()));
                }
                body1_statements.push(Statement::Assume(Expr {
                    kind: Box::new(ExprKind::Boolean(false)),
                    span: condition.span.clone(),
                    ty: Type::Bool,
                }));
    
                let body1 = Body {
                    statements: body1_statements,
                };
    
                let body2 = Body {
                    statements: vec![
                        Statement::Assume(Expr {
                            kind: Box::new(ExprKind::Unary(UOp::Not, condition.clone())),
                            span: condition.span.clone(),
                            ty: Type::Bool,
                        })
                    ],
                };
    
                transformed_statements.push(Statement::Choice(body1, body2));

                let choice_body = Body { statements: transformed_statements.clone() };
                *stmt = Statement::Choice(choice_body.clone(), choice_body.clone());
                
                // After transforming the current while, process its body for nested statements.
                Self::replace_while_in_body(&mut new_body, methods);
            },
            Statement::If(_, then_body, Some(else_body)) => {
                Self::replace_while_in_body(then_body, methods);
                Self::replace_while_in_body(else_body, methods);
            },
            Statement::Choice(body1, body2) => {
                Self::replace_while_in_body(body1, methods);
                Self::replace_while_in_body(body2, methods);
            },
            _ => {}
        }
    }
    
    fn replace_while_in_body(body: &mut Body, methods: &mut [Method]) {
        for stmt in &mut body.statements {
            Self::replace_while_recursive(stmt, methods);
        }
    }
    
    fn replace_while(doc: &mut Document) -> miette::Result<Document> {
        let mut methods: Vec<_> = doc.items.iter().cloned().filter_map(|item| {
            if let DocumentItem::Method(method) = item {
                Some(method)
            } else {
                None
            }
        }).collect();
        
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                if let Some(body) = &mut method.body {
                    Self::replace_while_in_body(body, &mut methods[..]);
                }
            }
        }
        Ok(doc.clone())
    }

}





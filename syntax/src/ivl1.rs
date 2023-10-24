use crate::ast::{
    // IF ELSE dependences
    Body, Document, DocumentItem, Expr, Specification, Statement, ExprKind, UOp, Op, Type, EK
};



/// 
#[derive(Debug)]
struct Encode1Context {
}

pub(crate) fn encode(doc: &Document) -> miette::Result<Document> {
    Encode1Context::encode(doc)
}

impl Encode1Context {
    fn encode(doc: & Document) -> miette::Result<Document> {
        let mut new_doc = doc.clone(); // Create a mutable copy of the document
        Self::add_output_decl(&mut new_doc)?;
        Self::add_input_decl(&mut new_doc)?;

        Self::replace_if(&mut new_doc)?;

        // Self::replace_assignments_with_assume(&mut new_doc)?;
        Self::replace_var_with_expr(&mut new_doc)?;

        Self::add_postcondition(&mut new_doc)?;
        Self::add_precondition(&mut new_doc)?;

        Ok(new_doc)
    }

    fn add_input_decl(doc: &mut Document) -> miette::Result<Document> {
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                if let Some(body) = &mut method.body {
                    for input_var in &method.inputs {
                        let var_decl = Statement::Var(input_var.clone(), None);
                        body.statements.insert(0, var_decl);
                    }
                }
            }
        }
        Ok(doc.clone())
    }

    fn add_output_decl(doc: &mut Document) -> miette::Result<Document> {
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                if let Some(body) = &mut method.body {
                    for input_var in &method.outputs {
                        let var_decl = Statement::Var(input_var.clone(), None);
                        body.statements.insert(0, var_decl);
                    }
                }
            }
        }
        Ok(doc.clone())
    }

    fn replace_if_recursive(stmt: &mut Statement) {
        match stmt {
            Statement::If(cond, then_body, else_body) => {
                Self::replace_if_in_body(then_body);
                if let Some(else_b) = else_body {
                    Self::replace_if_in_body(else_b);
                }
    
                let mut new_cond = cond.clone();
                new_cond.ty = Type::Bool;
    
                let assumption1 = Statement::Assume(new_cond.clone());
                then_body.statements.insert(0, assumption1);
    
                let not = UOp::Not;
                let expr = Expr {
                    kind: Box::new(ExprKind::Unary(not, new_cond)),
                    ty: Type::Bool,
                    // Assuming you have span info in your Expr, copy it over.
                    span: cond.span.clone(),
                };
                let assumption2 = Statement::Assume(expr);
                if let Some(else_b) = else_body {
                    else_b.statements.insert(0, assumption2);
                }
                else {
                    let mut new_statements = Vec::new();
                    new_statements.push(assumption2);
                    *else_body = Some(Body { statements: new_statements});
                }
    
                *stmt = Statement::Choice(then_body.clone(), else_body.clone().unwrap_or_default());
            },
            Statement::While { body: while_body, .. } => {
                Self::replace_if_in_body(while_body);
            },
            Statement::Choice(choice_body1, choice_body2) => {
                Self::replace_if_in_body(choice_body1);
                Self::replace_if_in_body(choice_body2);
            },
            _ => {}
        }
    }
    
    fn replace_if_in_body(body: &mut Body) {
        for stmt in &mut body.statements {
            Self::replace_if_recursive(stmt);
        }
    }
    
    fn replace_if(doc: &mut Document) -> miette::Result<Document> {
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                if let Some(body) = &mut method.body {
                    Self::replace_if_in_body(body);
                }
            }
        }
        Ok(doc.clone())
    }

    fn add_precondition(doc: &mut Document) -> miette::Result<Document> {
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                for spec in &mut method.specifications{
                    if let Specification::Requires(expr) = spec {
                        if let Some(body) = &mut method.body {
                            let assumption = Statement::Assume(expr.clone());

                            let mut index_after_var = 0;

                            for stmt in &mut body.statements{
                                if let Statement::Var(_var, _expr) = stmt { 
                                    index_after_var += 1;
                                }
                                else{
                                    break;
                                }
                            }
                            body.statements.insert(index_after_var, assumption);
                        }
                    }
                }
            }
        }
        Ok(doc.clone())
    }

    fn add_postcondition(doc: &mut Document) -> miette::Result<Document> {
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                for spec in &mut method.specifications{
                    if let Specification::Ensures(expr) = spec {
                        if let Some(body) = &mut method.body {
                            let assertion = Statement::Assert(expr.clone());
                            body.statements.push(assertion);
                        }
                    }
                }
            }
        }
        Ok(doc.clone())
    }



    // fn replace_assignments_with_assume_recursive(body: &mut Body) {
    //     let mut new_statements = Vec::new();
    //     for statement in &body.statements {
    //         match statement {
    //             Statement::Assignment(ident, expr) => {
    //                 let new_ident = Expr {
    //                     kind: Box::new(ExprKind::Var(ident.clone())),
    //                     span: expr.span.clone(),
    //                     ty: expr.ty.clone(),
    //                 };
    //                 let new_binary = ExprKind::Binary(new_ident.clone(), Op::Eq, expr.clone());
    //                 let mut new_expr = expr.clone();
    //                 new_expr.kind = Box::new(new_binary);
    //                 new_expr.ty = Type::Bool;
    //                 let assumption = Statement::Assume(new_expr);
    //                 new_statements.push(assumption);
    //             }
    //             Statement::If(expr, if_body, opt_else_body) => {
    //                 let mut new_if_body = if_body.clone();
    //                 Self::replace_assignments_with_assume_recursive(&mut new_if_body);
    //                 let new_else_body = opt_else_body.as_ref().map(|else_body| {
    //                     let mut cloned_else_body = else_body.clone();
    //                     Self::replace_assignments_with_assume_recursive(&mut cloned_else_body);
    //                     cloned_else_body
    //                 });
    //                 new_statements.push(Statement::If(expr.clone(), new_if_body, new_else_body));
    //             }
    //             Statement::While { condition, invariants, body } => {
    //                 let mut new_body = body.clone();
    //                 Self::replace_assignments_with_assume_recursive(&mut new_body);
    //                 new_statements.push(Statement::While {
    //                     condition: condition.clone(),
    //                     invariants: invariants.clone(),
    //                     body: new_body,
    //                 });
    //             }
    //             Statement::Choice(body1, body2) => {
    //                 let mut new_body1 = body1.clone();
    //                 let mut new_body2 = body2.clone();
    //                 Self::replace_assignments_with_assume_recursive(&mut new_body1);
    //                 Self::replace_assignments_with_assume_recursive(&mut new_body2);
    //                 new_statements.push(Statement::Choice(new_body1, new_body2));
    //             }
    //             _ => new_statements.push(statement.clone()),
    //         }
    //     }
    //     body.statements = new_statements;
    // }
    
    // fn replace_assignments_with_assume(doc: &mut Document) -> miette::Result<Document> {
    //     for item in &mut doc.items {
    //         if let DocumentItem::Method(method) = item {
    //             if let Some(body) = &mut method.body {
    //                 Self::replace_assignments_with_assume_recursive(body);
    //             }
    //         }
    //     }
    //     Ok(doc.clone())
    // }
    
    fn replace_var_with_expr_recursive(body: &mut Body) {
        let mut new_statements = Vec::new();
        for statement in &body.statements {
            match statement {
                Statement::Var(var, Some(expr)) => {
                    // Transform variable declaration with expression to Assume statement
                    let new_ident = Expr {
                        kind: Box::new(EK::Var(var.name.clone())),
                        span: expr.span.clone(),
                        ty: expr.ty.clone(),
                    };
                    let new_binary = EK::Binary(new_ident.clone(), Op::Eq, expr.clone());
                    let mut new_expr = expr.clone();
                    new_expr.kind = Box::new(new_binary);
                    new_expr.ty = Type::Bool;
                    let assignment = Statement::Assignment(var.name.clone(), expr.clone());
                    new_statements.push(Statement::Var(var.clone(), None));
                    new_statements.push(assignment);
                    
                }
                Statement::Choice(body1, body2) => {
                    let mut new_body1 = body1.clone();
                    let mut new_body2 = body2.clone();
                    Self::replace_var_with_expr_recursive(&mut new_body1);
                    Self::replace_var_with_expr_recursive(&mut new_body2);
                    new_statements.push(Statement::Choice(new_body1, new_body2));
                }
                _ => new_statements.push(statement.clone()),
            }
        }
        body.statements = new_statements;
    }
    
    fn replace_var_with_expr(doc: &mut Document) -> miette::Result<Document> {
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                if let Some(body) = &mut method.body {
                    Self::replace_var_with_expr_recursive(body);
                }
            }
        }
        Ok(doc.clone())
    }

}

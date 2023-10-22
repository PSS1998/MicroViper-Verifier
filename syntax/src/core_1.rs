use crate::ast::{
    // IF ELSE dependences
    Body, Document, DocumentItem, Expr, Specification, Statement, ExprKind, UOp, Op, Type
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
        Self::add_postcondition(&mut new_doc)?;
        Self::add_precondition(&mut new_doc)?;

        Self::replace_assignments_with_assume(&mut new_doc)?;

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

    fn replace_if(doc: &mut Document) -> miette::Result<Document>{
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                if let Some(body) = &mut method.body {
                    body.statements = body.statements.iter().map(|stmt| Self::encode_stmt(stmt)).collect();
                }
            }
        }
        Ok(doc.clone())
        
    }

    fn encode_stmt(stmt: &Statement) -> Statement {
        match stmt {
            Statement::If(cond, then, else_) => {
                let mut _then = then.clone();
                let mut _else_ = else_.clone().unwrap();
                
                let mut new_cond = cond.clone();
                new_cond.ty = Type::Bool;
                let assumption1 = Statement::Assume(new_cond);
                _then.statements.insert(0, assumption1);

                let not = UOp::Not;
                let mut new_cond2 = cond.clone();
                new_cond2.ty = Type::Bool;
                let mut expr = cond.clone();
                expr.kind = Box::new(ExprKind::Unary(not,new_cond2));
                expr.ty = Type::Bool;
                let assumption2 = Statement::Assume(expr.clone());
                _else_.statements.insert(0, assumption2);

                Statement::Choice(_then, _else_)
            },
            st => {
                (st).clone()
            }
        }
    }

    fn insert_precondition_recursive(body: &mut Body, assumption: &Statement) {
        let mut index_after_var = 0;
    
        for stmt in &mut body.statements {
            if let Statement::Var(_, _) = stmt {
                index_after_var += 1;
            } else {
                break;
            }
        }
    
        body.statements.insert(index_after_var, assumption.clone());
    
        // Handle nested control structures.
        for stmt in &mut body.statements {
            match stmt {
                Statement::If(_, if_body, opt_else_body) => {
                    Self::insert_precondition_recursive(if_body, assumption);
                    if let Some(else_body) = opt_else_body {
                        Self::insert_precondition_recursive(else_body, assumption);
                    }
                },
                Statement::While { body: while_body, .. } => {
                    Self::insert_precondition_recursive(while_body, assumption);
                },
                Statement::Choice(choice_body1, choice_body2) => {
                    Self::insert_precondition_recursive(choice_body1, assumption);
                    Self::insert_precondition_recursive(choice_body2, assumption);
                },
                _ => {}
            }
        }
    }
    
    fn add_precondition(doc: &mut Document) -> miette::Result<Document> {
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                for spec in &mut method.specifications {
                    if let Specification::Requires(expr) = spec {
                        let assumption = Statement::Assume(expr.clone());
                        if let Some(body) = &mut method.body {
                            Self::insert_precondition_recursive(body, &assumption);
                        } else {
                            todo!() // Handle the case where there's no body.
                        }
                    }
                }
            }
        }
        Ok(doc.clone())
    }

    fn insert_postcondition_recursive(body: &mut Body, assertion: &Statement) {
        // Handle nested control structures.
        for stmt in &mut body.statements {
            match stmt {
                Statement::If(_, if_body, opt_else_body) => {
                    Self::insert_postcondition_recursive(if_body, assertion);
                    if let Some(else_body) = opt_else_body {
                        Self::insert_postcondition_recursive(else_body, assertion);
                    }
                },
                Statement::While { body: while_body, .. } => {
                    // We typically don't insert postconditions inside a loop since it's semantically incorrect. 
                    // Instead, postconditions would apply after the loop finishes.
                    // However, if there are cases where you would want to do so, uncomment the next line.
                    // Self::insert_postcondition_recursive(while_body, assertion);
                },
                Statement::Choice(choice_body1, choice_body2) => {
                    Self::insert_postcondition_recursive(choice_body1, assertion);
                    Self::insert_postcondition_recursive(choice_body2, assertion);
                },
                _ => {}
            }
        }
    
        // Insert the assertion at the end of the body.
        body.statements.push(assertion.clone());
    }
    
    fn add_postcondition(doc: &mut Document) -> miette::Result<Document> {
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                for spec in &mut method.specifications {
                    if let Specification::Ensures(expr) = spec {
                        let assertion = Statement::Assert(expr.clone());
                        if let Some(body) = &mut method.body {
                            Self::insert_postcondition_recursive(body, &assertion);
                        } else {
                            todo!() // Handle the case where there's no body.
                        }
                    }
                }
            }
        }
        Ok(doc.clone())
    }

    fn replace_assignments_with_assume_recursive(body: &mut Body) {
        let mut new_statements = Vec::new();
        for statement in &body.statements {
            match statement {
                Statement::Assignment(ident, expr) => {
                    let new_ident = Expr {
                        kind: Box::new(ExprKind::Var(ident.clone())),
                        span: expr.span.clone(),
                        ty: expr.ty.clone(),
                    };
                    let new_binary = ExprKind::Binary(new_ident.clone(), Op::Eq, expr.clone());
                    let mut new_expr = expr.clone();
                    new_expr.kind = Box::new(new_binary);
                    new_expr.ty = Type::Bool;
                    let assumption = Statement::Assume(new_expr);
                    new_statements.push(assumption);
                }
                Statement::If(expr, if_body, opt_else_body) => {
                    let mut new_if_body = if_body.clone();
                    Self::replace_assignments_with_assume_recursive(&mut new_if_body);
                    let new_else_body = opt_else_body.as_ref().map(|else_body| {
                        let mut cloned_else_body = else_body.clone();
                        Self::replace_assignments_with_assume_recursive(&mut cloned_else_body);
                        cloned_else_body
                    });
                    new_statements.push(Statement::If(expr.clone(), new_if_body, new_else_body));
                }
                Statement::While { condition, invariants, body } => {
                    let mut new_body = body.clone();
                    Self::replace_assignments_with_assume_recursive(&mut new_body);
                    new_statements.push(Statement::While {
                        condition: condition.clone(),
                        invariants: invariants.clone(),
                        body: new_body,
                    });
                }
                Statement::Choice(body1, body2) => {
                    let mut new_body1 = body1.clone();
                    let mut new_body2 = body2.clone();
                    Self::replace_assignments_with_assume_recursive(&mut new_body1);
                    Self::replace_assignments_with_assume_recursive(&mut new_body2);
                    new_statements.push(Statement::Choice(new_body1, new_body2));
                }
                _ => new_statements.push(statement.clone()),
            }
        }
        body.statements = new_statements;
    }
    
    fn replace_assignments_with_assume(doc: &mut Document) -> miette::Result<Document> {
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                if let Some(body) = &mut method.body {
                    Self::replace_assignments_with_assume_recursive(body);
                }
            }
        }
        Ok(doc.clone())
    }
    


}

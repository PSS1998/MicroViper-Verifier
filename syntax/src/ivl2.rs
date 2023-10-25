use crate::ast::{
    Body, Document, DocumentItem, Expr, Specification, Statement, Var, Ident, EK, Method
};


/// 
#[derive(Debug)]
struct Encode2Context {
}

pub(crate) fn encode(doc: &Document) -> miette::Result<Document> {
    Encode2Context::encode(doc)
}

impl Encode2Context {
    fn encode(doc: & Document) -> miette::Result<Document> {
        let mut new_doc = doc.clone(); // Create a mutable copy of the document
        new_doc = Self::replace_methods(&mut new_doc)?;

        Ok(new_doc)
    }

    fn havoc_variable(var: &Var) -> (Statement, Var) {
        let var_declaration = Statement::Var(var.clone(), None);
        (var_declaration, var.clone())
    }

    fn replace_methods_recursive(stmt: &mut Statement, methods: &mut [Method]) {
        match stmt {
            Statement::MethodAssignment(idents, method_name, args) => {
                if let Some(index) = methods.iter().position(|m| m.name.text == method_name.text) {
                    let method = &mut methods[index];
                    let mut replaced_statements = Vec::new();
    
                    // for input parameters
                    for (index, var) in method.inputs.iter().enumerate() {
                        let replaced_function_input_variable = Ident {
                            text: format!("{}_input_var_{}", method.name.text, index),
                            span: var.name.span.clone(),
                        };
                        replaced_statements.push(Statement::Var(Var {
                            name: replaced_function_input_variable.clone(),
                            ty: var.ty.clone(),
                        }, Some(args[index].clone())));
                        
                        for spec in &mut method.specifications {
                            match spec {
                                Specification::Requires(expr) => {
                                    *expr = Self::replace_variable(expr.clone(), &var.name, &replaced_function_input_variable);
                                    replaced_statements.push(Statement::Assert(expr.clone()));
                                },
                                _ => {}
                            }
                        }
                    }                    
    
                    // Havoc return variables
                    for index in 0..idents.len() {
                        replaced_statements.push(Self::havoc_variable(&Var{name: idents[index].clone(), ty: method.outputs[index].ty}).0);
                    }
    
                    // for output parameters and ensures
                    for (index, var) in method.outputs.iter().enumerate() {
                        for spec in &mut method.specifications {
                            if let Specification::Ensures(expr) = spec {
                                *expr = Self::replace_variable(expr.clone(), &var.name, &idents[index]);
                                for (index, var) in method.inputs.iter().enumerate() {
                                    let replaced_function_input_variable = Ident {
                                        text: format!("{}_input_var_{}", method.name.text, index),
                                        span: var.name.span.clone(),
                                    };
                                    
                                    *expr = Self::replace_variable(expr.clone(), &var.name, &replaced_function_input_variable);
                                }     
                                replaced_statements.push(Statement::Assume(expr.clone()));
                            }
                        }
                    }
    
                    let choice_body = Body { statements: replaced_statements.clone() };
                    *stmt = Statement::Choice(choice_body.clone(), choice_body.clone());
                }
            },
            Statement::If(_, then_body, Some(else_body)) => {
                Self::replace_methods_in_body(then_body, methods);
                Self::replace_methods_in_body(else_body, methods);
            },
            Statement::Choice(body1, body2) => {
                Self::replace_methods_in_body(body1, methods);
                Self::replace_methods_in_body(body2, methods);
            },
            _ => {}
        }
    }
    
    fn replace_methods_in_body(body: &mut Body, methods: &mut [Method]) {
        for stmt in &mut body.statements {
            Self::replace_methods_recursive(stmt, methods);
        }
    }

    fn replace_methods(doc: &mut Document) -> miette::Result<Document> {
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
                    Self::replace_methods_in_body(body, &mut methods[..]);
                }
            }
        }

        Ok(doc.clone())
    }

    fn replace_variable(expr: Expr, from: &Ident, to: &Ident) -> Expr {
        let mut new_expr = expr;
        
        match *new_expr.kind {
            EK::Var(ref mut ident) if ident.text == from.text => {
                ident.text = to.text.clone();
            }
            EK::Unary(_, ref mut inner_expr) => {
                let cloned_inner = (*inner_expr).clone();
                *inner_expr = *Box::new(Self::replace_variable(cloned_inner, from, to));
            }
            EK::Binary(ref mut left, _, ref mut right) => {
                let cloned_left = (*left).clone();
                let cloned_right = (*right).clone();
                *left = *Box::new(Self::replace_variable(cloned_left, from, to));
                *right = *Box::new(Self::replace_variable(cloned_right, from, to));
            }
            EK::Call(_, ref mut args) => {
                for arg in args.iter_mut() {
                    let cloned_arg = (*arg).clone();
                    *arg = Self::replace_variable(cloned_arg, from, to);
                }
            }
            EK::Quantification(_, _, ref mut inner_expr) => {
                let cloned_inner = (*inner_expr).clone();
                *inner_expr = *Box::new(Self::replace_variable(cloned_inner, from, to));
            }
            _ => {}
        }
        
        new_expr
    }
 
}





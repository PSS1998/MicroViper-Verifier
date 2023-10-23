use crate::ast::{
    // IF ELSE dependences
    Body, Document, DocumentItem, Expr, Specification, Statement, ExprKind, UOp, Var, Span, Ident, Type::Int,Type, EK, Method
};
use std::collections::HashMap;
use std::collections::HashSet;
use std::cmp::Ord;
use regex::Regex;

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

        // TODO: replace all havoc variables
        // new_doc = Self::replace_all_havoc_variables(&mut new_doc)?;

        Ok(new_doc)
    }

    fn havoc_variable(var: &Ident) -> (Statement, Var) {
        let havoc_ident = Ident {
            text: format!("havoc_{}", var.text),
            span: var.span.clone(),
        };
        let havoc_var = Var {
            name: havoc_ident,
            ty: Type::Int, // TODO: should be the same type as the variable
        };
        let var_declaration = Statement::Var(havoc_var.clone(), None);
        (var_declaration, havoc_var)
    }

    fn replace_methods_recursive(stmt: &mut Statement, methods: &mut [Method]) {
        match stmt {
            Statement::MethodAssignment(idents, method_name, args) => {
                if let Some(index) = methods.iter().position(|m| m.name.text == method_name.text) {
                    let mut method = &mut methods[index];
                    let mut replaced_statements = Vec::new();
    
                    // for input parameters
                    for (index, var) in method.inputs.iter().enumerate() {
                        let replaced_function_input_variable = Ident {
                            text: format!("replaced_function_input_variable_{}", index),
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
                    for ident in &mut *idents {
                        replaced_statements.push(Self::havoc_variable(ident).0);
                    }
    
                    // for output parameters and ensures
                    let output_offset = method.inputs.len();
                    for (index, var) in method.outputs.iter().enumerate() {
                        for spec in &mut method.specifications {
                            if let Specification::Ensures(expr) = spec {
                                let replaced_function_output_variable = Ident {
                                    text: format!("replaced_function_output_variable_{}", output_offset + index),
                                    span: var.name.span.clone(),
                                };
                                *expr = Self::replace_variable(expr.clone(), &var.name, &idents[index]);
                                for (index, var) in method.inputs.iter().enumerate() {
                                    let replaced_function_input_variable = Ident {
                                        text: format!("replaced_function_input_variable_{}", index),
                                        span: var.name.span.clone(),
                                    };
                                    
                                    *expr = Self::replace_variable(expr.clone(), &var.name, &replaced_function_input_variable);
                                }     
                                replaced_statements.push(Statement::Assume(expr.clone()));
                            }
                        }
                    }
    
                    let true_expr = Expr {
                        kind: Box::new(ExprKind::Boolean(true)),
                        span: Span::zero(), // TODO: fix this
                        ty: Type::Bool,
                    };
                    *stmt = Statement::If(true_expr, Body { statements: replaced_statements }, None);
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





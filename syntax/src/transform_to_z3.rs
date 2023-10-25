use z3::{ast::Bool, ast::Int, SatResult, ast::Ast, Context};
use crate::ast::{Document, DocumentItem, Statement, Expr, Op, UOp, Type, EK, Body, Span};

use std::collections::HashSet;
use std::collections::HashMap;


/// 
#[derive(Debug)]
struct TransformToZ3 {
}

pub(crate) fn encode(doc: &Document, source: &str) -> miette::Result<Document> {
    TransformToZ3::encode(doc, source)
}

#[derive(Clone, Debug)]
enum Z3Ast<'a> {
    Bool(Bool<'a>),
    Int(Int<'a>),
}

impl TransformToZ3 {
    fn encode(doc: & Document, source: &str) -> miette::Result<Document> {        
        let mut new_doc = doc.clone(); // Create a mutable copy of the document

        let assumes_and_asserts_map = Self::collect_assumes_and_asserts(&mut new_doc)?;

        let mut spans: Vec<Span> = Vec::new();

        let mut all_methods_verified = true;
        for (_method_name, paths) in &assumes_and_asserts_map {

            for assumes_and_asserts in paths {

                let cfg = z3::Config::new();
                let ctx = z3::Context::new(&cfg);
                let solver = z3::Solver::new(&ctx);
                
                let variables = Self::extract_variables(&assumes_and_asserts);
                let mut z3_vars = HashMap::new();
                for var_name in variables.iter() {
                    z3_vars.insert(var_name.clone(), Int::new_const(&ctx, &**var_name));
                }

                let final_conditions = Self::translate_statements_to_z3(&assumes_and_asserts, &ctx);
                let final_conditions_span = Self::extract_spans_from_statements(&assumes_and_asserts);

                if final_conditions.is_empty() {
                    // If there are no Assert statements, return the combined Assume conditions.
                    let final_conditions = Self::translate_statements_with_no_assert_to_z3(&assumes_and_asserts, &ctx);

                    let condition = &final_conditions[0];
                    match condition {
                        Z3Ast::Bool(cond) => {
                            println!("{cond:#?}");
                            solver.push(); 
                            solver.assert(cond);
                            match solver.check() {
                                SatResult::Sat => {}, // The combined Assume conditions are satisfiable
                                _ => {
                                    all_methods_verified = false;
                                },
                            }
                            solver.pop(1); 
                        }
                        _ => panic!("Expected Bool type for final condition"),
                    }
                } else {
                    let mut all_unsatisfiable = true; // assume all conditions are unsatisfiable to begin with
                    
                    for (index, condition) in final_conditions.iter().enumerate() {
                        match condition {
                            Z3Ast::Bool(cond) => {
                                let not_condition = !cond.clone(); // Negate the condition
                                println!("{not_condition:#?}");
                                solver.push(); 
                                solver.assert(&not_condition);
                                match solver.check() {
                                    SatResult::Unsat => {} // This is expected, continue to the next condition
                                    SatResult::Sat => {
                                        all_unsatisfiable = false;
                                        spans.push(final_conditions_span[index].clone());
                                    }
                                    _ => {
                                        println!("Unknown result from solver");
                                        all_unsatisfiable = false;
                                        spans.push(final_conditions_span[index].clone());
                                    }
                                }
                                solver.pop(1); 
                            }
                            _ => panic!("Expected Bool type for final condition"),
                        }
                    }
                    
                    if !all_unsatisfiable {
                        all_methods_verified = false;
                    } 
                }
            }
        }

        if all_methods_verified {
            println!("Verified");
        } else {
            println!("Unverified");
            let mut unique_spans = Vec::new();
            let mut seen = HashSet::new();
            for span in spans.iter() {
                if seen.insert(span.clone()) {
                    unique_spans.push(span.clone());
                }
            }
            spans = unique_spans;
            // println!("{spans:#?}");
            for span in spans {
                Self::print_error_in_source(source, span)
            }
        }

        Ok(new_doc)

    }

    fn extract_variables(statements: &Vec<Statement>) -> HashSet<String> {
        let mut variables = HashSet::new();
        for statement in statements {
            match statement {
                Statement::Assert(expr) | Statement::Assume(expr) => {
                    Self::collect_variables_from_expr(expr, &mut variables);
                }
                _ => {}
            }
        }
        variables
    }
    
    fn collect_variables_from_expr(expr: &Expr, vars: &mut HashSet<String>) {
        match &*expr.kind {
            EK::Var(ident) => {
                vars.insert(ident.text.clone());
            }
            EK::Unary(_, subexpr) => {
                Self::collect_variables_from_expr(subexpr, vars);
            }
            EK::Binary(lhs, _, rhs) => {
                Self::collect_variables_from_expr(lhs, vars);
                Self::collect_variables_from_expr(rhs, vars);
            }
            _ => {}
        }
    }

    fn collect_from_body(body: &Body, prev_statements: &mut Vec<Statement>) -> Vec<Vec<Statement>> {
        let mut paths: Vec<Vec<Statement>> = Vec::new();
    
        let current_statements = prev_statements.clone();
    
        for stmt in &body.statements {
            match stmt {
                Statement::Assert(_) | Statement::Assume(_) => {
                    if paths.is_empty() {
                        let mut path: Vec<Statement> = Vec::new();
                        path.push(stmt.clone());
                        paths.push(path);
                    } else {
                        for path in &mut paths {
                            path.push(stmt.clone());
                        }
                    }
                },
                Statement::Choice(choice_body1, choice_body2) => {
                    let mut paths1 = Self::collect_from_body(choice_body1, &mut current_statements.clone());
                    let mut paths2 = Self::collect_from_body(choice_body2, &mut current_statements.clone());

                    if paths.is_empty(){
                        paths.extend(paths1);
                        paths.extend(paths2);
                    }
                    else{


                        let mut paths_copy = paths.clone();
                        paths.clear();

                        for path1 in &mut paths1 {
                            for path_copy in &mut paths_copy {
                                let mut new_path = path_copy.clone();
                                new_path.extend(path1.clone());
                                paths.push(new_path.clone());
                            }
                        }
                        for path2 in &mut paths2 {
                            for path_copy in &mut paths_copy {
                                let mut new_path = path_copy.clone();
                                new_path.extend(path2.clone());
                                paths.push(new_path.clone());
                            }
                        }
                    }

                },
                _ => {}
            }
        }
    
        paths
    }
    
    fn collect_assumes_and_asserts(doc: &mut Document) -> miette::Result<HashMap<String, Vec<Vec<Statement>>>> {
        let mut method_statements: HashMap<String, Vec<Vec<Statement>>> = HashMap::new();
    
        for item in &doc.items {
            if let DocumentItem::Method(method) = item {
                let mut prev_statements: Vec<Statement> = Vec::new();
    
                let paths = if let Some(body) = &method.body {
                    Self::collect_from_body(body, &mut prev_statements)
                } else {
                    Vec::new()
                };
                
                method_statements.insert(method.name.text.clone(), paths);
            }
        }
        
        Ok(method_statements)
    }

    fn translate_expr_to_z3<'a>(expr: &Expr, ctx: &'a Context) -> Z3Ast<'a> {
        match &*expr.kind {
            EK::Boolean(b) => Z3Ast::Bool(Bool::from_bool(ctx, *b)),
            EK::Integer(i) => Z3Ast::Int(Int::from_str(ctx, &i.to_string()).unwrap()),
            EK::Var(ident) => {
                match expr.ty {
                    Type::Bool => Z3Ast::Bool(Bool::new_const(ctx, &*ident.text)),
                    Type::Int => Z3Ast::Int(Int::new_const(ctx, &*ident.text)),
                    // _ => panic!("Unsupported type for variable {}", ident.text),
                }
            },
            EK::Unary(op, subexpr) => {
                let translated_subexpr = Self::translate_expr_to_z3(subexpr, ctx);
                match op {
                    UOp::Minus => Z3Ast::Int(-match translated_subexpr {
                        Z3Ast::Int(val) => val,
                        _ => panic!("Expected Int type"),
                    }),
                    UOp::Not => Z3Ast::Bool(!match translated_subexpr {
                        Z3Ast::Bool(val) => val,
                        _ => panic!("Expected Bool type"),
                    }),
                }
            }
            EK::Binary(lhs, op, rhs) => {
                let translated_lhs = Self::translate_expr_to_z3(lhs, ctx);
                let translated_rhs = Self::translate_expr_to_z3(rhs, ctx);
                match op {
                    Op::And => Z3Ast::Bool(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Bool(lhs_val), Z3Ast::Bool(rhs_val)) => lhs_val & rhs_val,
                        _ => panic!("Expected Bool types for And operation"),
                    }),
                    Op::Or => Z3Ast::Bool(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Bool(lhs_val), Z3Ast::Bool(rhs_val)) => lhs_val | rhs_val,
                        _ => panic!("Expected Bool types for Or operation"),
                    }),
                    Op::Eq => Z3Ast::Bool(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Bool(lhs_val), Z3Ast::Bool(rhs_val)) => lhs_val._eq(&rhs_val),
                        (Z3Ast::Int(lhs_val), Z3Ast::Int(rhs_val)) => lhs_val._eq(&rhs_val),
                        _ => panic!("Incompatible types for Eq operation"),
                    }),
                    Op::Gt => Z3Ast::Bool(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Int(lhs_val), Z3Ast::Int(rhs_val)) => lhs_val.gt(&rhs_val),
                        _ => panic!("Expected Int types for Gt operation"),
                    }),
                    Op::Lt => Z3Ast::Bool(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Int(lhs_val), Z3Ast::Int(rhs_val)) => lhs_val.lt(&rhs_val),
                        _ => panic!("Expected Int types for Lt operation"),
                    }),
                    Op::Geq => Z3Ast::Bool(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Int(lhs_val), Z3Ast::Int(rhs_val)) => lhs_val.ge(&rhs_val),
                        _ => panic!("Expected Int types for Geq operation"),
                    }),
                    Op::Leq => Z3Ast::Bool(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Int(lhs_val), Z3Ast::Int(rhs_val)) => lhs_val.le(&rhs_val),
                        _ => panic!("Expected Int types for Leq operation"),
                    }),
                    Op::Plus => Z3Ast::Int(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Int(lhs_val), Z3Ast::Int(rhs_val)) => lhs_val + rhs_val,
                        _ => panic!("Expected Int types for Plus operation"),
                    }),
                    Op::Minus => Z3Ast::Int(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Int(lhs_val), Z3Ast::Int(rhs_val)) => lhs_val - rhs_val,
                        _ => panic!("Expected Int types for Minus operation"),
                    }),
                    Op::Times => Z3Ast::Int(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Int(lhs_val), Z3Ast::Int(rhs_val)) => lhs_val * rhs_val,
                        _ => panic!("Expected Int types for Times operation"),
                    }),
                    Op::Divide => Z3Ast::Int(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Int(lhs_val), Z3Ast::Int(rhs_val)) => lhs_val / rhs_val,
                        _ => panic!("Expected Int types for Divide operation"),
                    }),
                    Op::Implies => Z3Ast::Bool(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Bool(lhs_val), Z3Ast::Bool(rhs_val)) => !lhs_val | rhs_val, // Implies can be translated to `!lhs || rhs`
                        _ => panic!("Expected Bool types for Implies operation"),
                    }),
                    Op::Neq => Z3Ast::Bool(match (translated_lhs, translated_rhs) {
                        (Z3Ast::Bool(lhs_val), Z3Ast::Bool(rhs_val)) => {
                            let eq_expr = lhs_val._eq(&rhs_val);
                            eq_expr.not()
                        },
                        (Z3Ast::Int(lhs_val), Z3Ast::Int(rhs_val)) => {
                            let eq_expr = lhs_val._eq(&rhs_val);
                            eq_expr.not()
                        },
                        _ => panic!("Incompatible types for Neq operation"),
                    }),
                    // _ => panic!("Unsupported binary operation"),
                }
            }
            _ => panic!("Unsupported expression kind"),
        }
    }

    fn translate_statements_to_z3<'a>(statements: &Vec<Statement>, ctx: &'a Context) -> Vec<Z3Ast<'a>> {
        let mut assume_conditions: Vec<Bool<'a>> = Vec::new();
        let mut result_conditions: Vec<Z3Ast<'a>> = Vec::new();
    
        for statement in statements {
            match statement {
                Statement::Assume(expr) => {
                    let z3_expr = Self::translate_expr_to_z3(expr, ctx);
                    let assume_val = match &z3_expr {
                        Z3Ast::Bool(val) => val.clone(),
                        _ => {
                            panic!("Expected a boolean expression");
                        },
                    };
                    assume_conditions.push(assume_val);
                }
                Statement::Assert(expr) => {
                    let z3_expr = Self::translate_expr_to_z3(expr, ctx);
                    let assert_val = match &z3_expr {
                        Z3Ast::Bool(val) => val,
                        _ => panic!("Expected a boolean expression"),
                    };
    
                    let combined_assume = assume_conditions.iter().fold(Bool::from_bool(ctx, true), |acc, x| Bool::and(ctx, &[&acc, x]));
                    result_conditions.push(Z3Ast::Bool(combined_assume.implies(assert_val)));

                    // Add the assertion condition to the list of assumptions after processing the assertion
                    assume_conditions.push(assert_val.clone());
                }
                _ => {} 
            }
        }

        result_conditions
    }

    fn extract_spans_from_statements(statements: &Vec<Statement>) -> Vec<Span> {
        let mut result_spans: Vec<Span> = Vec::new();
        
        for statement in statements {
            match statement {
                // When you encounter an Assert statement, push its span to the results.
                Statement::Assert(expr) => {
                    result_spans.push(expr.span.clone());
                }
                _ => {} 
            }
        }
    
        result_spans
    }

    fn translate_statements_with_no_assert_to_z3<'a>(statements: &Vec<Statement>, ctx: &'a Context) -> Vec<Z3Ast<'a>> {
        let mut assume_conditions: Vec<Bool<'a>> = Vec::new();
    
        for statement in statements {
            match statement {
                Statement::Assume(expr) => {
                    let z3_expr = Self::translate_expr_to_z3(expr, ctx);
                    let assume_val = match &z3_expr {
                        Z3Ast::Bool(val) => val.clone(),
                        _ => panic!("Expected a boolean expression"),
                    };
                    assume_conditions.push(assume_val);
                }
                _ => {}
            }
        }
    
        // Combine all the Assume conditions.
        let combined_assume = assume_conditions.iter().fold(Bool::from_bool(ctx, true), |acc, x| Bool::and(ctx, &[&acc, x]));
        vec![Z3Ast::Bool(combined_assume)]
    }

    fn print_error_in_source(source: &str, span: Span) {
        // Split source into lines
        let lines: Vec<&str> = source.split('\n').collect();
    
        // Find which line the error starts on
        let mut current_position = 0;
        for (line_number, line) in lines.iter().enumerate() {
            // Check if the error starts on this line
            if current_position + line.len() >= span.start() {
                // Print the line with the error
                println!("Line {}: {}", line_number + 1, line);
    
                // Print an indicator of where the error is
                let start_column = span.start() - current_position;
                let end_column = usize::min(line.len(), span.end() - current_position);
                let indicator = " ".repeat(start_column) + &"^".repeat(end_column - start_column);
                println!("{}", indicator);
                break;
            }
    
            // Update the current position
            current_position += line.len() + 1; // +1 for newline character
        }
    }
    
}
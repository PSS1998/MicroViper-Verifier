use crate::ast::{
    // IF ELSE dependences
    Body, Document, DocumentItem, Expr, Specification, Statement,ExprKind,UOp,Var, Span, Ident,Type::Int,Type, Op, EK
};
use std::collections::HashMap;
use std::cmp::Ord;
use regex::Regex;

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
        new_doc = Self::freshAssign(&mut new_doc)?;


        Self::replace_assignments_with_assume(&mut new_doc)?;
        // Self::replace_var_with_expr_with_assume(&mut new_doc)?;

        Ok(new_doc)
    }


    // initializes all variables declared at beginning of method to version 0
    fn freshAssign(doc: &mut Document) -> miette::Result<Document> {
        
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                if let Some(body) = &mut method.body {
                    let mut hashmap : HashMap<String,i32> = HashMap::new();
                    let mut statements =  Self::getFreshStatements(& body.statements, &mut hashmap);
                    body.statements = statements;
                   
                    // in case of Choice
                }
            }
        }
        Ok(doc.clone())
    }


    fn getFreshStatements(statements: & Vec<Statement>, vars_hashmap: &mut HashMap<String,i32>) -> Vec<Statement> {
        let mut fresh_statements = Vec::new();

        for statement in statements.iter() {

            if let Statement::Var ( var, expr ) = statement {
                 
                // initialize new variable
                //let mut new_var = Self::syncVar(var.clone(),&mut vars_hashmap);
                let mut new_var = var.clone();
                let var_name = var.name.text.to_string();
        
                // declaration case
                if !vars_hashmap.contains_key(&var_name) {
                    // Insert the variable into the hashmap with an initial count of 0.
                    vars_hashmap.insert(new_var.name.to_string(), 0);
                    new_var.name = Ident {
                        text: format!("{}0", var_name),
                        span: var.name.span, // Assuming the span remains the same
                    };
                }

                fresh_statements.push(Statement::Var(new_var,expr.clone()));
                //  WBU expression->                          ^^^^^^^^^^^    wbu function call????
            }

            if let Statement::Assignment(ident,expr) = statement {
                let ident_string = ident.text.to_string();

                let borrowed = vars_hashmap.clone();
                    if let Some(count) = vars_hashmap.get_mut(&ident_string) {

                        // Increment the counter for the variable.
                        *count += 1;

                        // Generate a new variable name with the count.
                        let new_ident = Ident {
                            text: format!("{}{}", ident_string, *count - 1),
                            span: ident.span, // Assuming the span remains the same
                        };
                        

                        // Push new declaration
                        let mut new_expr = expr.clone();
                        // new_expr = Encode0Context::syncExpression(&expr,&vars_hashmap);

                        new_expr = Encode0Context::syncExpression(&expr,&borrowed);


                        // Replace the assignment with the new variable name.
                        fresh_statements.push(Statement::Assignment(new_ident, new_expr));
                    }
            }
            
            if let Statement::Assume(expr) = statement {
                let mut new_expr = expr.clone();
                new_expr = Encode0Context::syncExpression(&expr,&vars_hashmap);
                fresh_statements.push(Statement::Assume(new_expr));
            }

            if let Statement::Assert(expr) = statement {
                let mut new_expr = expr.clone();
                new_expr = Encode0Context::syncExpression(&expr,&vars_hashmap);
                fresh_statements.push(Statement::Assert(new_expr));
            }

            if let Statement::Choice(body_if, body_else) = statement {
                let mut new_body_if = body_if.clone();
                let mut new_body_else = body_else.clone();

                let mut if_vars_hashmap = vars_hashmap.clone();
                let mut else_vars_hashmap = vars_hashmap.clone();

                new_body_if.statements = Encode0Context::getFreshStatements(& body_if.statements, &mut if_vars_hashmap);
                new_body_else.statements = Encode0Context::getFreshStatements(& body_else.statements, &mut else_vars_hashmap);

                // MOVE ALL DECLARATIONS TO THE BEGINNING
                // NOTE: add sync in the shortest body !!!!!!!!!!!!!!!!!!!!!

                // synchronizing vars
                for var in vars_hashmap.keys(){
                    let var_count_if = if_vars_hashmap.get(var); 
                    let var_count_else = if_vars_hashmap.get(var);
                    
                    // let max = var_count_else.max(var_count_if);
                    if let Some(max) = var_count_else.max(var_count_if){

                        // set variable name
                        let var_name = format!("{}{}", var, max);
                        // find variable type
                        let var_type = Encode0Context::getTypeFromVar(var.to_string(),fresh_statements.clone());
                        // declaration for the max version of the variable
                        let max_ver_decl = Statement::Var( Var{ name: var_name, ty:var_type }, None  );
                        //fresh_statements.insert(0, max_ver_decl)
                    }
                    
                    // TODO::::
                    // if var_count_if > var_count_else {
                    //     let assignment = Statement::Assignment(new_ident, new_expr);
                    //     new_body_else.statements.push(assignment);


                    //     // add the assignment at the end
                    //     //new_body_else.statements.insert(0,Statement::Var)
                    // }
                    // else if  var_count_if < var_count_else{
                    //     let assignment = Statement::Assignment(new_ident, new_expr);
                    //     new_body_if.statements.push(assignment);
                    //     // declaration for the max version of the variable

                    //     // add the assignment at the end
                    //     //new_body_else.statements.insert(0,Statement::Var)
                    // } 

                    // // update vars_hashmap

                    // if let Some(count) = vars_hashmap.get_mut(&ident_string) {

                    //     // Increment the counter for the variable.
                    //     *count += 1;
                    // }





                }


                fresh_statements.push(Statement::Choice(new_body_if,new_body_else));

            }

        }

        fresh_statements

    }


    fn getTypeFromVar(name: String, statements: Vec<Statement>) -> Type {
        let mut var_type = Type::Int;

        for stm in statements.iter() {
            if let Statement::Var(var , _) = stm {
                let tmp = var.clone();
                let (var_name, _ ) = Encode0Context::split_string(&tmp.name.text.to_string());

                if var_name == name {
                    var_type = tmp.ty;
                }
                break;
            }
        }

        var_type
    }

    fn split_string(input: &str) -> (String, String) {
        // Define a regular expression pattern to capture the word and number parts
        let re = Regex::new(r"([a-zA-Z]+)(\d+)").unwrap();
    
        // Use the regex to capture the parts
        if let Some(captures) = re.captures(input) {
            let word = captures.get(1).map_or("", |m| m.as_str()).to_string();
            let number = captures.get(2).map_or("", |m| m.as_str()).to_string();
            (word, number)
        } else {
            // If there is no match, return the whole input as the word and an empty string as the number
            (input.to_string(), "".to_string())
        }
    }


    fn syncVar(var: Var, vars_hashmap: &mut HashMap<String, i32>) -> Var {

        let mut new_var = var.clone();
        let var_name = var.name.text.to_string();

        // declaration case
        if !vars_hashmap.contains_key(&var_name) {
            // Insert the variable into the hashmap with an initial count of 0.
            vars_hashmap.insert(new_var.name.to_string(), 0);
            new_var.name = Ident {
                text: format!("{}0", var_name),
                span: var.name.span, // Assuming the span remains the same
            };
        }

        // var in expression
        else {
            todo!();
        }

        new_var
        
    }


    fn syncExpression(exp: &Expr, vars_hashmap: &HashMap<String, i32>) -> Expr {
        // Helper function to update a variable name based on the HashMap
        fn updateVarName(var_name: &str, count: i32) -> String {
            
            // the variable is a method input 
            if count==0 {
                format!("{}{}", var_name, count)
            }
            else{
                format!("{}{}", var_name, count - 1)
            }
            
        }

        // Clone the expression
        let mut new_expr = exp.clone();

        // Process the expression recursively

        if let ExprKind::Var(ident) = &mut *new_expr.kind{
            if let Some(count) = vars_hashmap.get(&ident.text.to_string()) {
                // Update the variable name
                ident.text = updateVarName(&ident.text, *count );
            }
        }
        if let ExprKind::Call(ident,args) = &mut *new_expr.kind{
            let mut new_expr_vec = Vec::<Expr>::new();
            for arg in args.iter_mut() {
                // Process arguments
                new_expr_vec.push(Self::syncExpression(arg, vars_hashmap)) ;
            }
            let new_call = ExprKind::Call(ident.clone(),new_expr_vec);
            // or Box<ExprKind>::new(new_call)??
            new_expr.kind = Box::new(new_call);
        }

                        
        if let ExprKind::Unary(uop, expr) = &mut *new_expr.kind {
            // Process expression
            let new_unary = ExprKind::Unary(uop.clone(),Self::syncExpression(expr, vars_hashmap));
            // or Box<ExprKind>::new(new_unary)??
            new_expr.kind = Box::new(new_unary);
            }


        if let ExprKind::Binary(left, op, right) = &mut *new_expr.kind {
            // Process left and right subexpressions
            let new_left = Self::syncExpression(left, vars_hashmap);
            let new_right = Self::syncExpression(right, vars_hashmap);
            let new_binary = ExprKind::Binary(new_left,op.clone(),new_right);
            new_expr.kind = Box::new(new_binary)
        }

            // Quantification ????

        new_expr
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
    
    // fn replace_var_with_expr_with_assume_recursive(body: &mut Body) {
    //     let mut new_statements = Vec::new();
    //     for statement in &body.statements {
    //         match statement {
    //             Statement::Var(var, Some(expr)) => {
    //                 // Transform variable declaration with expression to Assume statement
    //                 let new_ident = Expr {
    //                     kind: Box::new(EK::Var(var.name.clone())),
    //                     span: expr.span.clone(),
    //                     ty: expr.ty.clone(),
    //                 };
    //                 let new_binary = EK::Binary(new_ident.clone(), Op::Eq, expr.clone());
    //                 let mut new_expr = expr.clone();
    //                 new_expr.kind = Box::new(new_binary);
    //                 new_expr.ty = Type::Bool;
    //                 let assumption = Statement::Assume(new_expr);
    //                 new_statements.push(assumption);
    //             }
    //             Statement::If(expr, if_body, opt_else_body) => {
    //                 let mut new_if_body = if_body.clone();
    //                 Self::replace_var_with_expr_with_assume_recursive(&mut new_if_body);
    //                 let new_else_body = opt_else_body.as_ref().map(|else_body| {
    //                     let mut cloned_else_body = else_body.clone();
    //                     Self::replace_var_with_expr_with_assume_recursive(&mut cloned_else_body);
    //                     cloned_else_body
    //                 });
    //                 new_statements.push(Statement::If(expr.clone(), new_if_body, new_else_body));
    //             }
    //             Statement::While { condition, invariants, body } => {
    //                 let mut new_body = body.clone();
    //                 Self::replace_var_with_expr_with_assume_recursive(&mut new_body);
    //                 new_statements.push(Statement::While {
    //                     condition: condition.clone(),
    //                     invariants: invariants.clone(),
    //                     body: new_body,
    //                 });
    //             }
    //             Statement::Choice(body1, body2) => {
    //                 let mut new_body1 = body1.clone();
    //                 let mut new_body2 = body2.clone();
    //                 Self::replace_var_with_expr_with_assume_recursive(&mut new_body1);
    //                 Self::replace_var_with_expr_with_assume_recursive(&mut new_body2);
    //                 new_statements.push(Statement::Choice(new_body1, new_body2));
    //             }
    //             _ => new_statements.push(statement.clone()),
    //         }
    //     }
    //     body.statements = new_statements;
    // }
    
    // fn replace_var_with_expr_with_assume(doc: &mut Document) -> miette::Result<Document> {
    //     for item in &mut doc.items {
    //         if let DocumentItem::Method(method) = item {
    //             if let Some(body) = &mut method.body {
    //                 Self::replace_var_with_expr_with_assume_recursive(body);
    //             }
    //         }
    //     }
    //     Ok(doc.clone())
    // }

}





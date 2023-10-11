use crate::ast::{
    // IF ELSE dependences
    Body, Document, DocumentItem, Expr, Specification, Statement,ExprKind,UOp
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
        Self::addOutputDecl(&mut new_doc)?;
        Self::addInputDecl(&mut new_doc)?;

        Self::replaceIf(&mut new_doc)?;

        Ok(new_doc)

        // let doc = Self::addOutputDecl(&Self::addInputDecl(doc)?)?;
        // Ok(Document{doc})
    }

    fn addInputDecl(doc: &mut Document) -> miette::Result<Document> {
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

    fn addOutputDecl(doc: &mut Document) -> miette::Result<Document> {
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

    fn replaceIf(doc: &mut Document) -> miette::Result<Document>{
        for item in &mut doc.items {
            if let DocumentItem::Method(method) = item {
                if let Some(body) = &mut method.body {
                    body.statements = body.statements.iter().map(|stmt| Self::encode_stmt(stmt)).collect();
                    
                    // for input_var in &method.outputs {
                    //     let var_decl = Statement::Choice(input_var.clone(), None);
                    //     body.statements.insert(0, var_decl);
                    // }
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
                
                let assumption1 = Statement::Assume(cond.clone());
                _then.statements.insert(0, assumption1);

                let not = UOp::Not;
                let mut expr = cond.clone();
                expr.kind = Box::new(ExprKind::Unary(not,cond.clone()));
                let assumption2 = Statement::Assume(expr.clone());
                _else_.statements.insert(0, assumption2);
                println!("{cond:#?}");
                // let cond_ty = self.encode_expr(Location::Expr, cond)?;
                // cond_ty.expect_ty(cond.span, Type::Bool)?;

                // let then = self.encode_body(then)?;

                // let else_ = if let Some(else_) = else_.as_ref() {
                //     Some(self.encode_body(else_)?)
                // } else {
                //     None
                // };
                Statement::Choice(_then, _else_)
            
            },
            st => {
                (st).clone()
            }
        }
    }


}

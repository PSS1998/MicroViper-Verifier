module Semantics

open System
open AST

let id x = x

let rec bodyDecls (b : body) =
    List.collect stmtDecls b
and stmtDecls = function
    | Var(v, _) -> [v]
    | Assert(_) -> []
    | Assume(_) -> []
    | Assignment(_, _) -> []
    | MethodAssignment(_, _, _) -> []
    | If(_, t, e) ->
        [Some t; e] |> List.choose id
        |> List.collect bodyDecls
    | While(_, _, b) -> bodyDecls b

type decl =
    | Input of var
    | Output of var
    | Variable of var
    | Quantifier of quantifier * var

type decls = Map<ident, decl>

type location =
    | Pred of bool * ty option
    | Expr

type scope = { methods: Map<ident, method>; functions: Map<ident, func>; ds: Map<ident, decl> }
type scope_item = IMethod of method | IFunc of func | IDecl of decl
let scopeAdd decl sc =
    let var =
        match decl with
        | Input var | Output var | Variable var | Quantifier(_, var) -> var
    { sc with ds=Map.add (fst var) decl sc.ds }
let lookup n sc =
    match (Map.tryFind n sc.methods, Map.tryFind n sc.functions, Map.tryFind n sc.ds) with
    | (Some x, _, _) -> Some (IMethod x)
    | (_, Some x, _) -> Some (IFunc x)
    | (_, _, Some x) -> Some (IDecl x)
    | _ -> None

let expectTy expected actual =
    if expected <> actual then failwithf "Expected type %A but found %A" expected actual

let ifNoError x rs = if Seq.exists Result.isError rs then Error () else Ok x

let rec analyzeExpr sc (loc : location) =
    function
    | Boolean _ -> Bool
    | Integer _ -> Int
    | Result ->
        match loc with
        | Pred(false, Some ty) -> ty
        | _ -> failwithf "The 'result' expression is used outside of function postcondiiton"
    | Ref name ->
        match lookup name sc with
        | Some (IDecl dec) ->
            match (loc, dec) with
            | Pred(true, _), Output var -> failwithf "Output '%A' in precondition" var
            | Pred(true, _), Variable var -> failwithf "Variable '%A' in precondition" var
            | _, (Input dec | Output dec | Variable dec | Quantifier(_, dec)) -> snd dec
        | _ -> failwithf "Undeclared variable '%A'" var
    | Call(f, inputs) ->
        match lookup f sc with
        | Some (IFunc f) ->
            if List.length f.inputs <> List.length inputs then
                failwithf "Function '%A' given %A arguments, but expects %A" f (List.length inputs) (List.length f.inputs)
            for (input, x) in List.zip f.inputs inputs do
                let t = analyzeExpr sc Expr x
                expectTy (snd input) t

            f.ret
        | Some (IMethod m) -> failwithf "Tried to call method '%A' in expression position. Only funciton calls are allow here" m
        | Some (IDecl m) -> failwithf "Tried to call variable '%A'. Only funciton calls are allow here" m
        | None -> failwithf "Method '%A' does not exist" f
    | Unary(op, x) ->
        let operandTy, outTy =
            match op with
            | UMinus -> Int, Int
            | Neg -> Bool, Bool

        analyzeExpr sc loc x |> expectTy operandTy

        outTy
    | Binary(op, l, r) ->
        match (loc, op) with
        | Expr, Implies -> failwithf "Implication (==>) used in expression position, which is illegal"
        | _ -> ()

        let operandTy, outTy =
            match op with
            | And | Or | Implies -> Bool, Bool
            | Neq | Eq | Geq | Leq | Gt | Lt -> Int, Bool
            | Divide | Minus | Plus | Times -> Int, Int

        analyzeExpr sc loc l |> expectTy operandTy
        analyzeExpr sc loc r |> expectTy operandTy

        outTy
    | Quantification(q, free, inner) ->
        if loc = Expr then failwithf "Quantifiers '%A' are not allowed in expression position" free

        match lookup (fst free) sc with
        | Some _ -> failwithf "Variable '%A' declared multiple times" free
        | None -> ()

        analyzeExpr (scopeAdd (Quantifier(q, free)) sc) Expr inner |> expectTy Bool

        Bool


let analyzeSpec ret_ty sc = function
    | Requires e -> analyzeExpr sc (Pred(true, ret_ty)) e |> expectTy Bool
    | Ensures e -> analyzeExpr sc (Pred(false, ret_ty)) e |> expectTy Bool

let rec analyzeStmt sc : (statement -> statement * scope) = function
    | Var(v, e) ->
        match lookup (fst v) sc with
        | Some _ -> failwithf "Variable '%A' declared multiple times" v
        | None -> ()

        let newSc = scopeAdd (Variable v) sc

        match e with
        | Some e -> analyzeExpr newSc Expr e |> expectTy (snd v)
        | None -> ()

        (Var(v, e), newSc)
    | Assert e ->
        analyzeExpr sc (Pred(false, None)) e |> expectTy Bool
        (Assert e, sc)
    | Assume e ->
        analyzeExpr sc (Pred(false, None)) e |> expectTy Bool
        (Assume e, sc)
    | Assignment(name, expr) ->
        let var =
            match lookup name sc with
            | Some (IDecl (Variable var | Output var)) -> var
            | Some var -> failwithf "Bad assignment, tried to assign to '%A'" var
            | None -> failwithf "Bad assignment, tried to assign to undefined"
        analyzeExpr sc (Pred(false, None)) expr |> expectTy (snd var)
        (Assignment(name, expr), sc)
    | MethodAssignment(vars, name, args) ->
        match lookup name sc with
        | Some (IFunc _) -> analyzeStmt sc (Assignment(List.head vars, Call(name, args)))
        | Some (IMethod m) ->
            let varTys = List.map (fun var ->
                match lookup var sc with
                | Some (IDecl dec) ->
                    match dec with
                    | Output v | Variable v -> snd v
                    | Input var -> failwithf "Tried to assign to input parameter '%A'" var
                    | Quantifier(_, x) -> failwithf "Tried to assign to quanified variable '%A'" x
                | _ -> failwithf "Assignment to non-variable variable '%A'" var) vars

            if List.length m.inputs <> List.length args then
                failwithf "Method '%A' given %A arguments, but expects %A" m (List.length args) (List.length m.inputs)
            for (arg, x) in List.zip m.inputs args do
                let t = analyzeExpr sc Expr x
                expectTy (snd arg) t

            for (output, var) in List.zip m.outputs varTys do
                expectTy (snd output) var

            (MethodAssignment(vars, name, args), sc)
        | _ -> failwithf "Tried to call non-method '%A'" name
    | If(cond, t, e) ->
        analyzeExpr sc Expr cond |> expectTy Bool
        let t = analyzeBody sc t
        match e with
        | Some b -> (If(cond, t, Some (analyzeBody sc b)), sc)
        | None -> (If(cond, t, e), sc)
    | While(cond, invs, b) ->
        analyzeExpr sc Expr cond |> expectTy Bool
        for e in invs do analyzeExpr sc (Pred(false, None)) e |> expectTy Bool
        let b = analyzeBody sc b
        (While(cond, invs, b), sc)
and analyzeBody sc body : body =
    List.fold (fun (stmts, sc) s -> let stmt, sc = analyzeStmt sc s in (stmts@[stmt], sc)) ([], sc) body |> fst

let analyzeMethod sc (m : method) =
    let ds =
        [m.inputs, Input; m.outputs, Output]
            |> List.collect (fun (xs, f) -> List.map (fun v -> (fst v, f v)) xs)
            |> Map.ofList
    let sc = { sc with ds=ds }

    let toSet = List.map fst >> Set.ofSeq
    let dups = (toSet m.inputs, toSet m.outputs) ||> Set.intersect

    for d in dups do
        failwithf "Variable '%A' declared both as input and output" d

    for spec in m.specifications do analyzeSpec None sc spec
    match m.body with
    | Some b ->
        let decls = bodyDecls b
        let set = Set.ofList decls
        if List.length decls <> Set.count set then
            for d in set do
                if List.length (List.filter ((=) d) decls) > 1 then
                    failwithf "Variable '%A' declared multiple times" d
        { m with body=Some (analyzeBody sc b) }
    | None -> m

let analyzeFunction sc (f : func) =
    let ds =
        [f.inputs, Input]
            |> List.collect (fun (xs, f) -> List.map (fun v -> (fst v, f v)) xs)
            |> Map.ofList
    let sc = { sc with ds=ds }

    for spec in f.specifications do analyzeSpec (Some f.ret) sc spec
    match f.body with
    | Some b ->
        analyzeExpr sc Expr b |> expectTy f.ret
    | None -> ()

    f


let analyze (doc : document) =
    let methodName (m : method) = m.name
    let funcName (f : func) = f.name
    let methods = List.choose (function Method m -> Some (m.name, m) | _ -> None) doc |> Map.ofList
    let functions = List.choose (function Function f -> Some (f.name, f) | _ -> None) doc |> Map.ofList

    let sc : scope = { methods=methods; functions=functions; ds=Map.empty }

    List.map (function Method m -> Method (analyzeMethod sc m) | Function f -> Function (analyzeFunction sc f)) doc

open NUnit.Framework

let testParseErr src =
    match Parse.parse_src src with
    | Error _ -> ()
    | Ok _ -> Assert.Fail()
let testSemOk src =
    match Parse.parse_src src with
    | Error e -> raise e
    | Ok ast -> analyze ast |> ignore
let testSemErr src =
    match Parse.parse_src src with
    | Error e -> raise e
    | Ok ast ->
        try
            analyze ast |> ignore
            Assert.Fail()
        with _ -> ()


[<Test>]
let ``Output assignment``() =
    testSemOk "method test() returns (output: Int) { output := 42 }"
[<Test>]
let ``Input assignment``() =
    testSemErr "method test(input: Int) { input := 42 }"
[<Test>]
let ``Output in pred``() =
    testSemErr "method test() returns (output: Int) requires output > 0"
[<Test>]
let ``Undeclared method``() =
    testSemErr "method test() returns (output: Int) { output := ohNo() }"
[<Test>]
let ``Wrong input number``() =
    testSemErr "method hello(input: Int) returns (output: Int)
                method test() returns (output: Int) { output := hello() }"
[<Test>]
let ``Correct input number``() =
    testSemOk "method hello(input: Int) returns (output: Int)
               method test() returns (output: Int) { output := hello(12) }"
[<Test>]
let ``Wrong output number 1``() =
    testSemErr "method hello() returns (output: Int)
                method test() returns (a: Int, b: Int) { a, b := hello() }"
[<Test>]
let ``Wrong output number 2``() =
    testSemErr "method hello() returns (a: Int, b: Int)
               method test() returns (a: Int) { a := hello() }"
[<Test>]
let ``Correct output number 1``() =
    testSemOk "method hello() returns (a: Int)
               method test() returns (a: Int) { a := hello() }"
[<Test>]
let ``Correct output number 2``() =
    testSemOk "method hello() returns (a: Int, b: Int)
               method test() returns (a: Int, b: Int) { a, b := hello() }"
[<Test>]
let ``Quantifiers``() =
    testSemOk "method test() requires forall x: Int :: true"
[<Test>]
let ``Quantifier in expr``() =
    testSemErr "method test() { var x: Int := forall y: Int :: true }"
[<Test>]
let ``Quantifier clash``() =
    testSemErr "method test(x: Int) requires forall x: Int :: true"
[<Test>]
let ``Declared both input and output``() =
    testSemErr "method test(x: Int) returns (x: Int)"
[<Test>]
let ``Duplicate var in branches``() =
    testSemErr "method test() { if (true) { var x: Int } else { var x: Int } }"
[<Test>]
let ``Adding booleans``() =
    testSemErr "method test() { var x: Int := true + false }"
[<Test>]
let ``Anding ints``() =
    testSemErr "method test() { var x: Bool := 1 && 2 }"
[<Test>]
let ``Assign expr to multi``() =
    testParseErr "method test() returns (x: Int, y: Int) { x, y := 2 }"
[<Test>]
let ``Method not allowed in expr``() =
    testSemErr "method test() returns (x: Int) method using() returns (y: Int) { y := test() + 1 }"
[<Test>]
let ``Function allowed in expr``() =
    testSemOk "function test(): Int method using() returns (y: Int) { y := test() + 1 }"
[<Test>]
let ``Function assignment``() =
    testSemOk "function test(): Int method using() returns (y: Int) { y := test() }"
[<Test>]
let ``Imp in expr``() =
    testSemErr "function test(): Bool { true ==> false }"
[<Test>]
let ``Result in postcondition``() =
    testSemOk "function test(): Bool ensures result ==> true"
[<Test>]
let ``Result in precondition``() =
    testSemErr "function test(): Bool requires result ==> true"
[<Test>]
let ``Result in method postcondition``() =
    testSemErr "method test() ensures result ==> true"
[<Test>]
let ``Result in method precondition``() =
    testSemErr "method test() requires result ==> true"
[<Test>]
let ``Assert predicate``() =
    testSemOk "method test() { assert forall x: Int :: x > 0 }"
[<Test>]
let ``Assert integer``() =
    testSemErr "method test() { assert 3 }"
[<Test>]
let ``Assume predicate``() =
    testSemOk "method test() { assume forall x: Int :: x > 0 }"
[<Test>]
let ``Assume integer``() =
    testSemErr "method test() { assume 3 }"

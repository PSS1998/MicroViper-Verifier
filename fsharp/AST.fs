module AST

type ident = string

type uop = UMinus | Neg
type bop =
    | And
    | Divide
    | Eq
    | Geq
    | Gt
    | Implies
    | Leq
    | Lt
    | Minus
    | Neq
    | Or
    | Plus
    | Times

type ty = Int | Bool

type var = ident * ty

type quantifier = Exists | Forall

type expr =
    | Boolean of bool
    | Integer of string
    | Result
    | Ref of ident
    | Call of ident * expr list
    | Unary of uop * expr
    | Binary of bop * expr * expr
    | Quantification of quantifier * var * expr

type statement =
    | Var of var * expr option
    | Assert of expr
    | Assume of expr
    | Assignment of ident * expr
    | MethodAssignment of ident list * ident * expr list
    | If of expr * body * body option
    | While of expr * expr list * body
and body = statement list

type spec = Requires of expr | Ensures of expr

type method = {
    name: ident
    inputs: var list
    outputs: var list
    specifications: spec list
    body: body option
}
type func = {
    name: ident
    inputs: var list
    ret: ty
    specifications: spec list
    body: expr option
}

type document_item = Method of method | Function of func

type document = document_item list

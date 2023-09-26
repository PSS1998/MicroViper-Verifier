module Syntax

let private parse_with parse_fn src =
    match parse_fn src with
    | Ok ast -> Semantics.analyze ast
    | Error e -> raise e
let parse_src src = parse_with Parse.parse_src src
let parse_file path = parse_with Parse.parse_file path

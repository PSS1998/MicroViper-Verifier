module Parse

open FSharp.Text.Lexing
open System
open System.IO

exception ParseError of Position * string * Exception

let private parse (f : string option) parser src =
    let lexbuf = LexBuffer<char>.FromString src

    try
        Ok(parser Lexer.tokenize lexbuf)
    with
    | e ->
        let pos = lexbuf.EndPos
        let lastToken = new String(lexbuf.Lexeme)
        Console.ForegroundColor <- ConsoleColor.Red
        Console.WriteLine("[Parse error] {0}", e.Message)
        Console.ForegroundColor <- ConsoleColor.Yellow
        match f with
        | Some f -> Console.WriteLine(" + Parsing failed at {0}:{1}:{2}:", f, pos.Line, pos.Column)
        | None -> Console.WriteLine(" + Parsing failed at {0}:{1}:", pos.Line, pos.Column)
        Console.WriteLine(" + Last token: '{0}'", lastToken)
        Console.ResetColor()
        Error(ParseError(pos, lastToken, e))

let parse_src src = parse None Parser.start src
let parse_file f = File.ReadAllText f |> parse (Some f) Parser.start

open NUnit.Framework

[<Test>]
let ``Parse all examples``() =
    for dir in Directory.GetDirectories (Path.Join [|__SOURCE_DIRECTORY__; "examples"|]) do
        for f in Directory.GetFiles dir |> Array.filter (fun f -> f.EndsWith "vpr") do
            match parse_file f with
            | Error e -> raise e
            | _ -> ()

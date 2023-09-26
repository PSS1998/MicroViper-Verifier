open System
open Microsoft.Z3

let unwrap =
    function
    | Ok r -> r
    | Error e -> raise e

[<EntryPoint>]
let main(args) =
    // Parsing example
    for f in args do
        Console.WriteLine("Parsing: {0}", f)
        Console.WriteLine("{0}", Syntax.parse_file f)

    // Z3 usage example
    let ctx = new Context()
    let solver = ctx.MkSolver()

    let x = ctx.MkIntConst "x"
    let zero = ctx.MkInt 0

    let assumptions = [ctx.MkGt(x, zero)]
    // Uncomment this for an unsatisfiable set of assumptions
    // let assumptions = [ctx.MkGt(x, zero); ctx.MkLt(x, zero)]

    Console.WriteLine("Checking assumptions: {0}", assumptions)
    match solver.Check assumptions with
    | Status.UNSATISFIABLE ->
        Console.WriteLine(" + The assertions were unsatisfiable!")
        for unsat in solver.UnsatCore do
            Console.WriteLine(" + {0}", unsat);
    | Status.SATISFIABLE ->
        Console.WriteLine(" + The assertions were satisfiable!")
        Console.WriteLine(" + {0}", solver.Model);
    | _ -> ()

    0

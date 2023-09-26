# 02245-fsharp-template

A F# template for working on project A of the course [02245 - Program Verification](http://courses.compute.dtu.dk/02245/). It includes a parser and static analyzer for the input format *MicroViper* (see example below).

```vpr
method sum(n: Int) returns (res: Int)
  requires 0 <= n
  ensures  res == n * (n + 1) / 2
{
  res := 0
  var i: Int := 0
  while(i <= n)
    invariant i <= (n + 1)
    invariant res == (i - 1) * i / 2
  {
    res := res + i
    i := i + 1
  }
}
```

## Getting started

For building the template, you will need to have the following installed:

- [.NET](https://dotnet.microsoft.com/en-us/download)

> On macOS `brew install dotnet-sdk` is recommended.

The project also uses [Z3](https://github.com/Z3Prover/z3), which may or may not be installed automatically by .NET.

With all the requirements installed, run the following to check that the tests pass:

```bash
$ # all tests should pass
$ dotnet test
```

To run the main program, which includes an example usage of Z3, run:

```bash
$ dotnet run
```

If Microsoft.Z3 is set up correctly you should be presented with a model satisfying the constraints specified in the example.

## Project structure

- The parser is written using [FsLexYacc](http://fsprojects.github.io/FsLexYacc/), which is a parser generator.
- Semantic analysis is done in `Semantics.fs`, which checks that the program is well typed, and that there are no assignments to illegal (e.g. undeclared) variables.
- `Program.fs` is the entry point for interacting with the project. Use `dotnet run` and pass a list of files to parse and analyze.
    - To parse and analyze all the included examples run
    ```bash
    $ dotnet run examples/**/*.vpr
    ```

![logo pjulia](pjulia.png)
## Dependencies
- `OCaml` to compile OCaml code.
- `Menhir` to generate the parser.
- `OCamllex` to generate the lexer.
- `ppx_deriving` to derive printers from types.
- `dune` to compile.
## Usage
### Using make
- `make`: Compile `pjuliac`. Compile and run `tests/test.jl`
- `make interp`: Compile `pjuliac` and interpret `tests/test.jl`
- `make clean`: Clean compilation files.
- `make explain`: Explain grammar conflicts.
- `make tests`: Run all the tests.
- `make vtests`: Run all the tests (verbose).
- `make test1`: Run the parsing tests.
- `make vtest1`: Run the parsing tests (verbose).
- `make test2`: Run the typing tests.
- `make vtest2`: Run the typing tests (verbose).
- `make test3`: Run the compiling tests.
- `make vtest3`: Run the compiling tests (verbose).
### Using ./pjuliac
#### Syntax
`./pjuliac [option] file.jl`
#### Options
- `-help`: Display option list
- `--help`: Display option list
- `--parse-only`: Do just the parsing
- `--type-only`: Do just the parsing and typing
- `-i`: Do not compile but interpret
- `-n`: Number of random tests (default is 0) (Not implemented yet)
- `--print`: Print the parsed ast
- `--debug`: Print the tokens
## File list
### src
- `ast.ml`: Astract syntax tree.
- `dune`
- `dune-project`
- `env.ml`: Typing environment.
- `gen.ml`: Code generation.
- `interp.ml`: Interpret.
- `lexer.mll`
- `pjuliac.ml`: Main file.
- `tast.ml`: Typed abstract syntax tree.
- `type.ml`: Typing system.
- `type.mli`: Typing interface.
- `x86_64.ml`: Slightly modified version of JCF's module to write x86-64
assembly from OCaml.
## Added tests
### tests/
- `test.jl`
### tests/syntax/bad/
- `testfile-unclosed_string-2.jl`
- `testfile-illegal-char-2.jl`
### tests/typing/bad/
- `testfile-not-int.jl`
- `testfile-println-2.jl`
- `testfile-print-2.jl`
- `testfile-param-type.jl`
- `testfile-div-2.jl`
- `testfile-affect-1.jl`
- `testfile-affect-2.jl`
- `testfile-affect-3.jl`
- `testfile-affect-4.jl`
- `testfile-affect-5.jl`
- `testfile-local.jl`
- `testfile-return-2.jl`
- `testfile-for-1.jl`
- `testfile-for-2.jl`
- `testfile-for-3.jl`
- `testfile-for-4.jl`
- `testfile-for-5.jl`
- `testfile-while-1.jl`
- `testfile-while-2.jl`
- `testfile-while-3.jl`
- `testfile-while-4.jl`
- `testfile-issou.jl`
- `testfile-depth.jl`
### tests/exec/
- `depth.jl`
- `fib_rec.jl`
- `jpp.jl`
- `local.jl`
- `print_return.jl`
- `struct2.jl`
- `syracuse.jl`
- `while2.jl`
- `while3.jl`
- `for6.jl`
- `for7.jl`
- `print.jl`
- `z.jl`

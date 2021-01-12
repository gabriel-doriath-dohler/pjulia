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

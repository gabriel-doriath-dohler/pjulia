![logo pjulia](pjulia.png)
## Dependencies
- `OCaml` to compile OCaml code.
- `Menhir` to generate the parser.
- `OCamllex` to generate the lexer.
- `ppx_deriving` to derive printers from types.
- `dune` to compile.
## Usage
- `make`: Compile and run `tests/test.jl`
- `make clean`: Clean compilation files.
- `make explain`: Explain grammar conflicts.
- `make tests`: Run all the tests.
- `make vtests`: Run all the tests (verbose).
- `make test1`: Run the parsing tests.
- `make vtest1`: Run the parsing tests (verbose).
- `make test2`: Run the typing tests.
- `make vtest2`: Run the typing tests (verbose).

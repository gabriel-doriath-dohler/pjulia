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
### tests/exec-fail/
- `local.jl`
## What has not been done
- While and for loops
- Structures
- Return
- Dynamic dyspatch
## Difficulties encountered
- It took use some time to properly modify the grammar to avoid conflicts. In particular we use a different idea as last time for the unitary minus.
- Understanding the scope of variables was challenging. As a result, we had to change the
typing environment (from/to mutable to/from imutable) a few times.
- We missunderstood the way that values are represented in pjulia so we had to
modify most of the compiler at the last minute. We should have read the guidlines more
carefully.
## Bonus
- We have started to create an interpret in order to check the compiler against it.
- We have added some tests (see above).
- We have added a function `typeof` to get the type of an objet as an int. This
would have been usefull for the dynamic dispatch.
- We have tried to provide good error messages during execution. And do so in a
modular way.
- We have coded an iterative fast expodentiation.
## Lexing
We accept any char in a comment (except the newline character).
## Parsing
We parser used to contain bugs. Some are fixed now. It will generate a warning
`unused var zz`. This is ugly but expected.
## Typing
We use `src/env.ml` as a typing environment. This makes everything clearer. The
Typing system has been rewritten fromm the ground up compared to last time. We
use a reference to modify the behavior of `type1_expr` so it can be used in
part 2. No structure can be called `typeof`, `div`, `print` or `println`.
Structures and functions can't have the same name. We use hashtables instead of
Set as this is faster.
## pjuliac
If multiple files are inputed to `pjuliac`, the last one will be the only one
proceced.
## X86-64
We have added conditional moves.
## Code generation
We mostly produce non factorised code. This is a bit easier and faster if the
code still fits in the cache.

We interpret `a == b == c` as `(a == b) == c`. We behavior of `return` outside
a function is unspecified.
## Conclusion
We are a bit sad that we did not finish the project in time but we still
learned a lot and had fun.

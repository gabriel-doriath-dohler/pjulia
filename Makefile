all: pjuliac.exe
	dune exec src/./pjuliac.exe -- tests/test.jl

pjuliac.exe:
	dune build src/pjuliac.exe

explain: pjuliac.exe
	menhir --base /tmp/parser --dump --explain src/parser.mly
	cat /tmp/parser.conflicts

tests: pjuliac.exe test1

vtests: pjuliac.exe vtest1

test1: pjuliac.exe
	./tests/run_tests -1 "dune exec src/./pjuliac.exe --"

vtest1: pjuliac.exe
	./tests/run_tests -v1 "dune exec src/./pjuliac.exe --"

clean:
	dune clean
	rm -f /tmp/parser.conflicts

.PHONY: all explain tests vtests test1 vtest1 clean

all: pjuliac.exe
	./pjuliac tests/test.jl
	gcc -no-pie tests/test.s
	./a.out

pjuliac.exe:
	dune build src/pjuliac.exe
	cp _build/default/src/pjuliac.exe pjuliac
	chmod +w pjuliac

explain: pjuliac.exe
	menhir --base /tmp/parser --dump --explain src/parser.mly
	cat /tmp/parser.conflicts

tests: pjuliac.exe test1 test2

vtests: pjuliac.exe vtest1 vtest2

test1: pjuliac.exe
	./tests/run_tests -1 "./pjuliac"

vtest1: pjuliac.exe
	./tests/run_tests -v1 "./pjuliac"

test2: pjuliac.exe
	./tests/run_tests -2 "./pjuliac"

vtest2: pjuliac.exe
	./tests/run_tests -v2 "./pjuliac.exe"

clean:
	dune clean
	rm -f /tmp/parser.conflicts
	rm -f pjuliac

.PHONY: all explain tests vtests test1 vtest1 test2 vtest2 clean

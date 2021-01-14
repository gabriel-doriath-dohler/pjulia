all: pjuliac.exe
	./pjuliac tests/test.jl
	gcc -no-pie tests/test.s
	./a.out

interp: pjuliac.exe
	./pjuliac -i tests/test.jl

pjuliac.exe:
	dune build src/pjuliac.exe
	cp _build/default/src/pjuliac.exe pjuliac
	chmod +w pjuliac

explain: pjuliac.exe
	menhir --base /tmp/parser --dump --explain src/parser.mly
	cat /tmp/parser.conflicts

tests: pjuliac.exe
	./tests/run_tests -all "./pjuliac"

vtests: pjuliac.exe
	./tests/run_tests -vall "./pjuliac"

test1: pjuliac.exe
	./tests/run_tests -1 "./pjuliac"

vtest1: pjuliac.exe
	./tests/run_tests -v1 "./pjuliac"

test2: pjuliac.exe
	./tests/run_tests -2 "./pjuliac"

vtest2: pjuliac.exe
	./tests/run_tests -v2 "./pjuliac"

test3: pjuliac.exe
	./tests/run_tests -3 "./pjuliac"

vtest3: pjuliac.exe
	./tests/run_tests -v3 "./pjuliac"

testi: pjuliac.exe
	./tests/run_tests -i "./pjuliac -i"

vtesti: pjuliac.exe
	./tests/run_tests -vi "./pjuliac -i"

clean:
	dune clean
	rm -f /tmp/parser.conflicts
	rm -f pjuliac
	rm -f *.s
	rm -f tests/*.s
	rm -f tests/exec/*.s
	rm -f tests/exec-fail/*.s
	rm -f out

.PHONY: all explain tests vtests test1 vtest1 test2 vtest2 clean

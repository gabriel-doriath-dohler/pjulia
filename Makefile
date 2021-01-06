all: pjuliac.exe
	dune exec src/./pjuliac.exe -- tests/test.jl

pjuliac.exe:
	dune build src/pjuliac.exe

explain: pjuliac.exe
	menhir --base /tmp/parser --dump --explain src/parser.mly
	cat /tmp/parser.conflicts

clean:
	dune clean
	rm -f /tmp/parser.conflicts

.PHONY: all explain clean

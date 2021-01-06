all: pjuliac.exe
	dune exec src/./pjuliac.exe -- tests/test.jl

pjuliac.exe:
	dune build src/pjuliac.exe

clean:
	dune clean

.PHONY: all clean

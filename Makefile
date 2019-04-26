all: calc.exe
run: calc.exe
	./calc -s default.st -d default.st
calc.exe: modNet.ml env.ml parser.mly lexer.mll calc.ml Makefile
	ocamlc -c modNet.ml 
	ocamlc -c env.ml
	ocamllex lexer.mll
	menhir --explain parser.mly
	dune build calc.exe
	rm -f calc
	ln -s _build/default/calc.exe calc

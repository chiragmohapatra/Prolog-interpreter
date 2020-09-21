output: Assign5.ml main.ml parser.mly lexer.mll
	ocamlc -c Assign5.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o output str.cma lexer.cmo Assign5.cmo parser.cmo main.cmo
	rm parser.mli lexer.ml parser.ml
	
run:
	./output input.pl

clean:
	rm *.cmi *.cmo output

all: main

main: ast.cmo simplification.cmo parser.cmo lexer.cmo main.cmo
	ocamlc -o main ast.cmo simplification.cmo parser.cmo lexer.cmo main.cmo

ast.cmo: ast.ml
	ocamlc -c ast.ml

simplification.cmo: simplification.ml
	ocamlc -c simplification.ml
	
lexer.cmo lexer.ml: lexer.mll parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml

parser.cmo parser.ml: parser.mly
	ocamlyacc parser.mly
	rm parser.mli		
	ocamlc -c parser.ml

main.cmo: main.ml
	ocamlc -c main.ml

clean:
	rm -f *.cm* lexer.ml parser.ml main

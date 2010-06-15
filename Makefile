all: main

main: ast.cmo qe.cmo lexer.cmo parser.cmo main.cmo
	ocamlc -o main ast.cmo qe.cmo lexer.cmo parser.cmo main.cmo

ast.cmo: ast.ml
	ocamlc -c ast.ml
	
qe.cmo: qe.ml
	ocamlc -c qe.ml
  
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
	rm *.cm* lexer.ml parser.ml main

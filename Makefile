OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=
OCAMLFLAGS=$(INCLUDES)
OCAMLOPTFLAGS=$(INCLUDES)

all: formulas
opt: formulas.opt

FORMULAS_OBJS=ast.cmo simplification.cmo parser.cmo lexer.cmo main.cmo

formulas: $(FORMULAS_OBJS)
	$(OCAMLC) -o formulas $(OCAMLFLAGS) $(FORMULAS_OBJS)

FORMULAS_OPT_OBJS=ast.cmx simplification.cmx parser.cmx lexer.cmx main.cmx

formulas.opt: $(FORMULAS_OPT_OBJS)
	$(OCAMLOPT) -o formulas.opt $(OCAMLFLAGS) $(FORMULAS_OPT_OBJS)

lexer.ml: lexer.mll parser.ml
	ocamllex lexer.mll

parser.ml: parser.mly
	ocamlyacc parser.mly
	rm parser.mli

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f formulas formulas.opt
	rm -f *.cm[iox] *.o
	rm -f parser.ml lexer.ml

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

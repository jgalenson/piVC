PROJ_DIR = . 

OCFLAGS = -dtypes -g

INCLUDES = $(PROJ_DIR:%=-I %) -I ../utils

OC = ocamlc $(OCFLAGS) $(INCLUDES)
OCAMLDEP = ocamldep
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex

include ../Makefile_includes

default :
	cd .. ;\
	make backend; \

all : depend language

language : $(LANGUAGE_COMPILE_OBJS)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly 

.ml.cmo:
	$(OC) -c $<

.mli.cmi:
	$(OC) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

clean :
	rm lexer.ml
	rm parser.ml
	rm $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.annot)

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

PROJ_DIR = . 

INCLUDES = $(PROJ_DIR:%=-I %) -I ../utils

OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex

include ../Makefile_vars
include ../Makefile_includes

default :
	cd .. ;\
	make backend; \

all : depend language

opt : depend language_opt

language : $(LANGUAGE_COMPILE_OBJS)

language_opt : $(LANGUAGE_COMPILE_OBJS_OPT)

.SUFFIXES: .mli .ml .cmi .cmo .cmx .mll .mly 

.ml.cmo:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

clean :
	rm -f lexer.ml
	rm -f parser.ml
	rm -f $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.cmx) $(PROJ_DIR:%=%/*.o) $(PROJ_DIR:%=%/*.annot)

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

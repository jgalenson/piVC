PROJ_DIR = .

INCLUDES = $(PROJ_DIR:%=-I %) -I ../utils -I ../language -I ../servers

include ../Makefile_vars
include ../Makefile_includes

default :
	cd .. ;\
	make backend; \

all : depend compiler

opt : depend compiler_opt

compiler : $(COMPILER_OBJS) test_parser.cmo
	$(OCAMLC) -o ../../bin/test_parser $(COMPILER_INCLUDES) $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) test_parser.cmo

compiler_opt : $(COMPILER_OBJS_OPT) test_parser.cmx
	$(OCAMLOPT) -o ../../bin/test_parser $(COMPILER_INCLUDES_OPT) $(UTILS_OBJS_OPT) $(LANGUAGE_OBJS_OPT) $(COMPILER_OBJS_OPT) test_parser.cmx

%.cmo : %.ml %.cmi
	$(OCAMLC) -c $<

%.cmx : %.ml %.cmi
	$(OCAMLOPT) -c $<

%.cmi : %.mli
	$(OCAMLC) -c $<

clean :
	rm -f $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.cmx) $(PROJ_DIR:%=%/*.o) $(PROJ_DIR:%=%/*.annot)
	rm -f ../../bin/test_parser

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
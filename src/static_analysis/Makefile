PROJ_DIR = . 

INCLUDES = $(PROJ_DIR:%=-I %) -I ../utils -I ../language -I ../compiler

include ../Makefile_vars
include ../Makefile_includes

default :
	cd .. ;\
	make backend; \

all : depend static_analysis

opt : depend static_analysis_opt

static_analysis : $(STATIC_ANALYSIS_OBJS)
	mkdir -p ../../bin
	$(OCAMLC) -o ../../bin/test_cfg str.cma unix.cma threads.cma $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) cfg.cmo

static_analysis_opt : $(STATIC_ANALYSIS_OBJS_OPT)
	mkdir -p ../../bin
	$(OCAMLOPT) -o ../../bin/test_cfg str.cmxa unix.cmxa threads.cmxa $(UTILS_OBJS_OPT) $(LANGUAGE_OBJS_OPT) $(COMPILER_OBJS_OPT) cfg.cmx

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.ml.cmo:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

clean :
	rm -f $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.cmx) $(PROJ_DIR:%=%/*.o) $(PROJ_DIR:%=%/*.annot)
	rm -f ../../bin/test_cfg

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

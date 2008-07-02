PROJ_DIR = . 

OCFLAGS = -dtypes -g -thread
OCOPTFLAGS = $(OCFLAGS) -p

INCLUDES = $(PROJ_DIR:%=-I %) -I ../utils -I ../language -I ../compiler

OCAML = ocamlc $(OCFLAGS) $(INCLUDES)
OCAMLOPT = ocamlopt $(OCOPTFLAGS) $(INCLUDES)
OCAMLDEP = ocamldep
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex

include ../Makefile_includes

default :
	cd .. ;\
	make backend; \

all : depend static_analysis

opt : depend static_analysis_opt

static_analysis : $(STATIC_ANALYSIS_OBJS)
	$(OCAML) -o ../../bin/test_cfg str.cma unix.cma threads.cma $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) cfg.cmo

static_analysis_opt : $(STATIC_ANALYSIS_OBJS_OPT)
	$(OCAMLOPT) -o ../../bin/test_cfg str.cmxa unix.cmxa threads.cmxa $(UTILS_OBJS_OPT) $(LANGUAGE_OBJS_OPT) $(COMPILER_OBJS_OPT) cfg.cmx

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.ml.cmo:
	$(OCAML) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

.mli.cmi:
	$(OCAML) -c $<

clean :
	rm -f $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.cmx) $(PROJ_DIR:%=%/*.o) $(PROJ_DIR:%=%/*.annot)
	rm -f ../../bin/test_cfg

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

PROJ_DIR = .

INCLUDES = $(PROJ_DIR:%=-I %)

include ../Makefile_vars
include ../Makefile_includes

default :
	cd .. ;\
	make backend; \

all : depend xmllight utils

opt : depend xmllight_opt utils_opt

utils : $(UTILS_OBJS)

utils_opt : $(UTILS_OBJS_OPT)

xmllight:
	cd xml-light; \
	make xml-light.cma; \

xmllight_opt:
	cd xml-light; \
	make xml-light.cmxa; \

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo : %.ml %.cmi
	$(OCAMLC) -c $<

%.cmi : %.mli
	$(OCAMLC) -c $<

%.cmx : %.ml %.cmi
	$(OCAMLOPT) -c $<

clean :
	cd xml-light; \
	make clean; \

	rm -f $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.cmx) $(PROJ_DIR:%=%/*.o) $(PROJ_DIR:%=%/*.annot)

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
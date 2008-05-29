PROJ_DIR = .

OCFLAGS = -dtypes -thread -g

INCLUDES = $(PROJ_DIR:%=-I %)

OC = ocamlc $(OCFLAGS) $(INCLUDES)
OCAMLDEP = ocamldep

include ../Makefile_includes

default :
	cd .. ;\
	make backend; \

all : depend xmllight utils

utils : $(UTILS_OBJS)

xmllight:
	cd xml-light; \
	make xml-light.cma; \

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo : %.ml %.cmi
	$(OC) -c $<

%.cmi : %.mli
	$(OC) -c $<

clean :
	cd xml-light; \
	make clean; \

	rm $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.annot)

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
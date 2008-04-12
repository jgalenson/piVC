PROJ_DIR = .

OCFLAGS = -dtypes -thread

INCDIR = $(PROJ_DIR:%=-I %)

OC = ocamlc $(OCFLAGS) $(INCDIR)
OCAMLDEP = ocamldep

include ../Makefile_includes

default :
	cd .. ;\
	make backend; \

all : depend netutils

netutils : $(UTILS_OBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo : %.ml %.cmi
	$(OC) -c $<

%.cmi : %.mli
	$(OC) -c $<

clean :
	rm $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.annot)

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
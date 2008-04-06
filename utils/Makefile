PROJ_DIR = .

OCFLAGS = -dtypes

INCDIR = $(PROJ_DIR:%=-I %)

OC = ocamlc $(OCFLAGS) $(INCDIR)
OCAMLDEP = ocamldep

NET_UTILS_OBJS = net_utils.cmo constants.cmo scanner.cmo utils.cmo

default : depend netutils

netutils : $(NET_UTILS_OBJS)

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
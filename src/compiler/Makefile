PROJ_DIR = .

OCFLAGS = -dtypes -thread

INCLUDES = $(PROJ_DIR:%=-I %) -I ../utils -I ../language -I ../servers

OC = ocamlc $(OCFLAGS) $(INCLUDES)
OCAMLDEP = ocamldep

include ../Makefile_includes

default :
	cd .. ;\
	make backend; \

all : depend compiler

compiler : $(COMPILER_OBJS) test_parser.cmo
	$(OC) -o test_parser $(COMPILER_INCLUDES) $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) test_parser.cmo

%.cmo : %.ml %.cmi
	$(OC) -c $<

%.cmi : %.mli
	$(OC) -c $<

clean :
	rm $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.annot)
	rm test_parser

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
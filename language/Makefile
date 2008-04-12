PROJ_DIR = . 

OCFLAGS = -dtypes

INCDIR = $(PROJ_DIR:%=-I %) -I ../utils

OC = ocamlc $(OCFLAGS) $(INCDIR)
OCAMLDEP = ocamldep

include ../Makefile_includes

default :
	cd .. ;\
	make backend; \

all : depend parser lexer language language_post

parser :
	ocamlyacc parser.mly;

lexer :
	ocamllex lexer.mll

language : $(LANGUAGE_OBJS)

language_post :
	$(OC) -c parser.mli;
	$(OC) -c parser.ml;
	$(OC) -c lexer.ml;
	rm lexer.ml;
	rm parser.mli;
	rm parser.ml;

%.cmo : %.ml %.cmi
	$(OC) -c $<

%.cmi : %.mli
	$(OC) -c $<

#%.cmo : %.mll
#	ocamllex $<
#	$(OC) -c $(*F).ml
#
#%.cmo : %.mly
#	ocamlyacc $<
#	$(OC) -c $(*F).ml

clean :
	rm $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.annot)

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
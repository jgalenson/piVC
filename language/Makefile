PROJ_DIR = . 

OCFLAGS = -dtypes

INCDIR = $(PROJ_DIR:%=-I %)

OC = ocamlc $(OCFLAGS) $(INCDIR)
OCAMLDEP = ocamldep

LANGUAGE_OBJS = ast.cmo scope_stack.cmo

default : depend parser lexer language language_post

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

clean :
	rm $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.annot)

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
PROJ_DIR = . 

CCFLAGS =
OCFLAGS =

INCDIR = $(PROJ_DIR:%=-I %)

CC = gcc $(CCFLAGS) $(INCDIR)
OC = ocamlc $(OCFLAGS) $(INCDIR)
JAVAC = javac

default : parser

parser :
	$(OC) -c ast.ml;

	ocamlyacc parser.mly;
	ocamllex lexer.mll;
	$(OC) -c global.ml;
	$(OC) -c parser.mli;
	$(OC) -c lexer.ml;
	$(OC) -c parser.ml;
	$(OC) -c test_parser.ml;
	$(OC) -c ast.ml;
	$(OC) -o test_parser global.cmo ast.cmo lexer.cmo parser.cmo test_parser.cmo

	rm lexer.ml;
	rm parser.mli;
	rm parser.ml;

clean :
	rm $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.cma) \
		$(PROJ_DIR:%=%/*.cmx) $(PROJ_DIR:%=%/*.o)
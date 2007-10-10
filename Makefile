PROJ_DIR = . client server dp_server

CCFLAGS =
OCFLAGS =

INCDIR = $(PROJ_DIR:%=-I %)

CC = gcc $(CCFLAGS) $(INCDIR)
OC = ocamlc $(OCFLAGS) $(INCDIR)

default : pivc

pivc : parser

parser :
	ocamllex server/lexer.mll;
	ocamlyacc server/parser.mly;
	$(OC) -c server/parser.mli;
	$(OC) -c server/parser.ml;
	$(OC) -o server/a.out server/lexer.ml;
	rm server/lexer.ml;
	rm server/parser.mli;
	rm server/parser.ml;

clean :
	rm $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.cma) \
		$(PROJ_DIR:%=%/*.cmx) $(PROJ_DIR:%=%/*.o) \
		$(PROJ_DIR:%=%/*.class)
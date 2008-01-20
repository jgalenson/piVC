PROJ_DIR = . 

CCFLAGS =
OCFLAGS = -dtypes

INCDIR = $(PROJ_DIR:%=-I %)

CC = gcc $(CCFLAGS) $(INCDIR)
OC = ocamlc $(OCFLAGS) $(INCDIR)
JAVAC = javac

default : parser

parser :

	ocamlyacc parser.mly;
	ocamllex lexer.mll;
	$(OC) -c utils.mli
	$(OC) -c ast.mli;
	$(OC) -c scope_stack.mli;
	$(OC) -c semantic_checking.mli;
	$(OC) -c parser.mli;
	$(OC) -c basic_paths.mli
	$(OC) -c parse_utils.mli;
	$(OC) -c verification_conditions.mli
	$(OC) -c test_parser.mli;
	$(OC) -c server.mli;
	$(OC) -c utils.ml
	$(OC) -c ast.ml;
	$(OC) -c scope_stack.ml;
	$(OC) -c semantic_checking.ml;
	$(OC) -c parser.ml;
	$(OC) -c lexer.ml;
	$(OC) -c parse_utils.ml;
	$(OC) -c expr_utils.ml;
	$(OC) -c basic_paths.ml;
	$(OC) -c verification_conditions.ml
	$(OC) -c test_parser.ml;
	$(OC) -I xml-light -c server.ml;
	$(OC) -o test_parser utils.cmo ast.cmo expr_utils.cmo basic_paths.cmo verification_conditions.cmo lexer.cmo parser.cmo scope_stack.cmo semantic_checking.cmo parse_utils.cmo test_parser.cmo 
	$(OC) -I xml-light -o server unix.cma xml-light.cma utils.cmo ast.cmo lexer.cmo parser.cmo scope_stack.cmo semantic_checking.cmo expr_utils.cmo verification_conditions.cmo basic_paths.cmo parse_utils.cmo str.cma server.cmo

	rm lexer.ml;
	rm parser.mli;
	rm parser.ml;

clean :
	rm $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.cma) \
		$(PROJ_DIR:%=%/*.cmx) $(PROJ_DIR:%=%/*.annot) $(PROJ_DIR:%=%/*.o)
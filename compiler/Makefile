PROJ_DIR = .

OCFLAGS = -dtypes

INCDIR = $(PROJ_DIR:%=-I %) -I ../language

OC = ocamlc $(OCFLAGS) $(INCDIR)
OCAMLDEP = ocamldep

COMPILER_OBJS = utils.cmo semantic_checking.cmo expr_utils.cmo basic_paths.cmo verification_conditions.cmo parse_utils.cmo test_parser.cmo utils.cmo
LANGUAGE_OBJS = ast.cmo lexer.cmo parser.cmo scope_stack.cmo

default : depend compiler

compiler : $(COMPILER_OBJS)
	$(OC) -o test_parser $(LANGUAGE_OBJS) $(COMPILER_OBJS)

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
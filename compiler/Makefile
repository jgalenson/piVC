PROJ_DIR = .

OCFLAGS = -dtypes -thread

INCDIR = $(PROJ_DIR:%=-I %) -I ../utils -I ../language -I ../servers

OC = ocamlc $(OCFLAGS) $(INCDIR)
OCAMLDEP = ocamldep

COMPILER_OBJS = semantic_checking.cmo expr_utils.cmo basic_paths.cmo verification_conditions.cmo parse_utils.cmo transform_yices.cmo counterexamples.cmo verify.cmo test_parser.cmo
COMPILER_INCLUDES = str.cma unix.cma threads.cma
UTILS_OBJS = net_utils.cmo constants.cmo scanner.cmo utils.cmo
LANGUAGE_OBJS = ast.cmo lexer.cmo parser.cmo scope_stack.cmo

default : depend compiler

compiler : $(COMPILER_OBJS)
	$(OC) -o test_parser $(COMPILER_INCLUDES) $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS)

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
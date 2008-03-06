PROJ_DIR = .

OCFLAGS = -dtypes

INCDIR = $(PROJ_DIR:%=-I %) -I ../language -I ../compiler -I xml-light

OC = ocamlc $(OCFLAGS) $(INCDIR)
OCAMLDEP = ocamldep

SERVER_OBJS = xml_generator.cmo server_framework.cmo
SERVER_INCLUDES = str.cma unix.cma xml-light.cma
COMPILER_OBJS = utils.cmo semantic_checking.cmo expr_utils.cmo basic_paths.cmo verification_conditions.cmo parse_utils.cmo utils.cmo
LANGUAGE_OBJS = ast.cmo lexer.cmo parser.cmo scope_stack.cmo

default : depend servers

servers : $(SERVER_OBJS) main_server.cmo dp_server.cmo
	$(OC) -o main_server $(LANGUAGE_OBJS) $(COMPILER_OBJS) $(SERVER_INCLUDES) $(SERVER_OBJS) main_server.cmo
	$(OC) -o dp_server $(LANGUAGE_OBJS) $(COMPILER_OBJS) $(SERVER_INCLUDES) $(SERVER_OBJS) dp_server.cmo

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo : %.ml %.cmi
	$(OC) -c $<

%.cmi : %.mli
	$(OC) -c $<

clean :
	rm $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.annot)
	rm main_server
	rm dp_server

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
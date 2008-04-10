PROJ_DIR = .

OCFLAGS = -dtypes -thread

INCDIR = $(PROJ_DIR:%=-I %) -I ../utils -I ../language -I ../compiler -I xml-light

OC = ocamlc $(OCFLAGS) $(INCDIR)
OCAMLDEP = ocamldep

UTILS_OBJS = net_utils.cmo constants.cmo scanner.cmo utils.cmo
SERVER_OBJS = xml_generator.cmo server_framework.cmo ci_yices.cmo main_server.cmo dp_server.cmo
SERVER_INCLUDES = str.cma unix.cma xml-light.cma threads.cma
COMPILER_OBJS = semantic_checking.cmo expr_utils.cmo basic_paths.cmo verification_conditions.cmo parse_utils.cmo transform_yices.cmo counterexamples.cmo verify.cmo
LANGUAGE_OBJS = ast.cmo lexer.cmo parser.cmo scope_stack.cmo

default : depend servers

servers : $(SERVER_OBJS) main_server_executable.cmo dp_server_executable.cmo both_servers.cmo
	$(OC) -o main_server $(SERVER_INCLUDES) $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) $(SERVER_OBJS) main_server_executable.cmo
	$(OC) -o dp_server $(SERVER_INCLUDES) $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) $(SERVER_OBJS) dp_server_executable.cmo
	$(OC) -o both_servers $(SERVER_INCLUDES) $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) $(SERVER_OBJS) both_servers.cmo

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo : %.ml %.cmi
	$(OC) -c $<

%.cmi : %.mli
	$(OC) -c $<

clean :
	rm $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.annot)
	rm main_server
	rm dp_server
	rm both_servers

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
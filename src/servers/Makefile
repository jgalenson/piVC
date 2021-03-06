PROJ_DIR = .

INCLUDES = $(PROJ_DIR:%=-I %) -I ../utils -I ../language -I ../compiler -I ../utils/xml-light

include ../Makefile_vars
include ../Makefile_includes

default :
	cd .. ;\
	make backend; \

all : depend servers

opt : depend servers_opt

servers : $(SERVER_OBJS) main_server_executable.cmo dp_server_executable.cmo both_servers.cmo
	mkdir -p ../../bin
	$(OCAMLC) -o ../../bin/main_server $(SERVER_INCLUDES) $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) $(SERVER_OBJS) main_server_executable.cmo
	$(OCAMLC) -o ../../bin/dp_server $(SERVER_INCLUDES) $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) $(SERVER_OBJS) dp_server_executable.cmo
	$(OCAMLC) -o ../../bin/both_servers $(SERVER_INCLUDES) $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) $(SERVER_OBJS) both_servers.cmo

servers_opt : $(SERVER_OBJS_OPT) main_server_executable.cmx dp_server_executable.cmx both_servers.cmx
	mkdir -p ../../bin
	$(OCAMLOPT) -o ../../bin/main_server $(SERVER_INCLUDES_OPT) $(UTILS_OBJS_OPT) $(LANGUAGE_OBJS_OPT) $(COMPILER_OBJS_OPT) $(SERVER_OBJS_OPT) main_server_executable.cmx
	$(OCAMLOPT) -o ../../bin/dp_server $(SERVER_INCLUDES_OPT) $(UTILS_OBJS_OPT) $(LANGUAGE_OBJS_OPT) $(COMPILER_OBJS_OPT) $(SERVER_OBJS_OPT) dp_server_executable.cmx
	$(OCAMLOPT) -o ../../bin/both_servers $(SERVER_INCLUDES_OPT) $(UTILS_OBJS_OPT) $(LANGUAGE_OBJS_OPT) $(COMPILER_OBJS_OPT) $(SERVER_OBJS_OPT) both_servers.cmx

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo : %.ml %.cmi
	$(OCAMLC) -c $<

%.cmx : %.ml %.cmi
	$(OCAMLOPT) -c $<

%.cmi : %.mli
	$(OCAMLC) -c $<

clean :
	rm -f $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.cmx) $(PROJ_DIR:%=%/*.o) $(PROJ_DIR:%=%/*.annot)
	rm -f ../../bin/main_server
	rm -f ../../bin/dp_server
	rm -f ../../bin/both_servers

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

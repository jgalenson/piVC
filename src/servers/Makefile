PROJ_DIR = .

OCFLAGS = -dtypes -thread

INCLUDES = $(PROJ_DIR:%=-I %) -I ../utils -I ../language -I ../compiler -I ../utils/xml-light

OC = ocamlc $(OCFLAGS) $(INCLUDES)
OCAMLDEP = ocamldep

include ../Makefile_includes

default :
	cd .. ;\
	make backend; \

all : depend servers

servers : $(SERVER_OBJS) main_server_executable.cmo dp_server_executable.cmo both_servers.cmo
	$(OC) -o ../../bin/main_server $(SERVER_INCLUDES) $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) $(SERVER_OBJS) main_server_executable.cmo
	$(OC) -o ../../bin/dp_server $(SERVER_INCLUDES) $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) $(SERVER_OBJS) dp_server_executable.cmo
	$(OC) -o ../../bin/both_servers $(SERVER_INCLUDES) $(UTILS_OBJS) $(LANGUAGE_OBJS) $(COMPILER_OBJS) $(SERVER_OBJS) both_servers.cmo

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo : %.ml %.cmi
	$(OC) -c $<

%.cmi : %.mli
	$(OC) -c $<

clean :
	rm $(PROJ_DIR:%=%/*.cmi) $(PROJ_DIR:%=%/*.cmo) $(PROJ_DIR:%=%/*.annot)
	rm ../../bin/main_server
	rm ../../bin/dp_server
	rm ../../bin/both_servers

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

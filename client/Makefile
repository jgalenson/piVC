PROJ_DIR = java_gui java_gui/data_structures

CCFLAGS =
OCFLAGS =

INCDIR = $(PROJ_DIR:%=-I %)

CC = gcc $(CCFLAGS) $(INCDIR)
OC = ocamlc $(OCFLAGS) $(INCDIR)
JAVAC = javac

default : gui

gui :

	$(JAVAC) java_gui/data_structures/*.java java_gui/*.java; \
 
clean :
	rm $(PROJ_DIR:%=%/*.class)

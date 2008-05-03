PROJ_DIR = java_gui java_gui/data_structures

CCFLAGS =
OCFLAGS =

INCDIR = $(PROJ_DIR:%=-I %)

CC = gcc $(CCFLAGS) $(INCDIR)
OC = ocamlc $(OCFLAGS) $(INCDIR)
JAVAC = javac

default : gui

gui :

	mkdir classes
	$(JAVAC) -classpath java_gui:java_gui/syntax.jar -d classes java_gui/data_structures/*.java java_gui/*.java;
	cd classes; jar cfe pivc.jar PiGui `find . -name "*.class"` ../java_gui/syntax.jar
	mv classes/pivc.jar ../../bin/PiGui.jar
	rm -rf classes

clean :
	rm ../../bin/PiGui.jar
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
	mkdir classes/images
	$(JAVAC) -classpath java_gui:../../include/java_gui/syntax.jar -d classes java_gui/data_structures/*.java java_gui/*.java
	cp Manifest.txt classes
	cd classes; jar cfm PiGui.jar Manifest.txt `find . -name "*.class"`
	mv classes/PiGui.jar ../../bin/PiGui.jar
	rm -rf classes

clean :
	rm ../../bin/PiGui.jar
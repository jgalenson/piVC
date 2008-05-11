PROJ_DIR = java_gui java_gui/data_structures

CCFLAGS =
OCFLAGS =

INCDIR = $(PROJ_DIR:%=-I %)

CC = gcc $(CCFLAGS) $(INCDIR)
OC = ocamlc $(OCFLAGS) $(INCDIR)
JAVAC = javac

default : gui

gui :

	mkdir temp_files_for_compilation
	$(JAVAC) -d temp_files_for_compilation -classpath java_gui:java_gui/libraries/* java_gui/data_structures/*.java java_gui/*.java
	cd temp_files_for_compilation; jar xf ../java_gui/libraries/*
	mkdir temp_files_for_compilation/images
	cp java_gui/images/*.jpg temp_files_for_compilation/images
	cp java_gui/MANIFEST.MF temp_files_for_compilation
	cd temp_files_for_compilation; jar cfm PiGui.jar MANIFEST.MF `find . -name "*.class"` images
	mv temp_files_for_compilation/PiGui.jar ../../bin/PiGui.jar
	rm -rf temp_files_for_compilation

clean :
	rm ../../bin/PiGui.jar
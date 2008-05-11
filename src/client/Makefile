PROJ_DIR = java_gui java_gui/data_structures

CCFLAGS =
OCFLAGS =

INCDIR = $(PROJ_DIR:%=-I %)

CC = gcc $(CCFLAGS) $(INCDIR)
OC = ocamlc $(OCFLAGS) $(INCDIR)
JAVAC = javac

default : gui

#These comments should really be in the block of code below, but comments are printed out during compiliation and pollute the output.

#mkdir -p makes a directory and does not error if that directory already exists (handy if a previous compilation failed for whatever reason)
#Currently, the script only copies jpgs into the jar, so this needs to be changed if new types of images are added.
#For security reasons, I didn't want to do a wildcard copy of files in the images directory, because that would include the
#.svn directory and potentially other bad stuff.

gui :

	mkdir -p temp_files_for_compilation
	$(JAVAC) -d temp_files_for_compilation -classpath java_gui:java_gui/libraries/* java_gui/data_structures/*.java java_gui/*.java
	cd temp_files_for_compilation; jar xf ../java_gui/libraries/*
	mkdir -p temp_files_for_compilation/images
	cp java_gui/images/*.jpg temp_files_for_compilation/images
	cp java_gui/MANIFEST.MF temp_files_for_compilation
	cd temp_files_for_compilation; jar cfm PiGui.jar MANIFEST.MF `find . -name "*.class"` images
	mv temp_files_for_compilation/PiGui.jar ../../bin/PiGui.jar
	rm -rf temp_files_for_compilation

clean :
	rm ../../bin/PiGui.jar
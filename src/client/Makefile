PROJ_DIR = java_gui java_gui/data_structures

INCLUDES = -I ../utils

include ../Makefile_vars
include ../Makefile_includes

default : 
	cd .. ;\
	make frontend; \

all : gui executable

opt : gui executable_opt

#Currently, the script only copies jpgs and pngs into the jar, so this needs to be changed if new types of images are added.


gui :
	mkdir -p temp_files_for_compilation
	$(JAVAC) -d temp_files_for_compilation -target 1.5 -classpath java_gui:java_gui/libraries/* java_gui/data_structures/*.java java_gui/*.java java_gui/org/syntax/jedit/*.java java_gui/org/syntax/jedit/tokenmarker/*.java
	cd temp_files_for_compilation; jar xf ../java_gui/libraries/*
	mkdir -p temp_files_for_compilation/images
	cp java_gui/images/*.jpg temp_files_for_compilation/images
	cp java_gui/images/*.png temp_files_for_compilation/images
	cp java_gui/MANIFEST.MF temp_files_for_compilation
	cp java_gui/ENVIRONMENT temp_files_for_compilation
	cd temp_files_for_compilation; jar cfm PiGui.jar MANIFEST.MF `find . -name "*.class"` images ENVIRONMENT
	mv temp_files_for_compilation/PiGui.jar ../../bin/PiGui.jar
	rm -rf temp_files_for_compilation

executable :
	$(OCAMLC) -c pi_gui.ml
	$(OCAMLC) -o ../../bin/pi_gui str.cma utils.cmo pi_gui.cmo

executable_opt :
	$(OCAMLOPT) -c pi_gui.ml
	$(OCAMLOPT) -o ../../bin/pi_gui str.cmxa utils.cmx pi_gui.cmx

clean :
	rm -f pi_gui.cmi pi_gui.cmo pi_gui.cmx pi_gui.o
	rm -f ../../bin/pi_gui
	rm -f ../../bin/PiGui.jar
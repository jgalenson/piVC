DIRS	= server client


PROJ_DIR = . client server dp_server client/java_gui

CCFLAGS =
OCFLAGS =

INCDIR = $(PROJ_DIR:%=-I %)

CC = gcc $(CCFLAGS) $(INCDIR)
OC = ocamlc $(OCFLAGS) $(INCDIR)
JAVAC = javac

default : make_all

make_all :
	cd server; \
	make; \

	cd client; \
	make; \


clean :
	cd server; \
	make clean; \

	cd client; \
	make clean; \

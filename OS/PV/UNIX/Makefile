RM_F = perl -MExtUtils::Command -e rm_f

CC = gcc
EXES = init father mother son daughter destroy 

all: $(EXES)

init: init.c app.h pv.o
	$(CC) -o $@ init.c pv.o

father: father.c app.h pv.o
	$(CC) -o $@ father.c pv.o

mother: mother.c app.h pv.o
	$(CC) -o $@ mother.c pv.o

son: son.c app.h pv.o
	$(CC) -o $@ son.c pv.o

daughter: daughter.c app.h pv.o
	$(CC) -o $@ daughter.c pv.o

pv.o: pv.h pv.c
	$(CC) -c pv.c

destroy: destroy.c app.h pv.o
	$(CC) -o $@ destroy.c pv.o

clean::
	$(RM_F) $(EXES) pv.o *.out report.pod *.exe *.tmp

doc: report.html Active.css all.png doc.png

report.html: report.pod
	podhtm -s Active.css -o $@ report.pod

report.pod: report.podx
	podx report.podx

all.png: Makefile
	gvmake -s 6x7

doc.png: Makefile
	gvmake -s 4x6 doc

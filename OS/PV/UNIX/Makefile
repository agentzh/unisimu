CC = gcc
EXES = init father mother son daughter

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

clean::
	rm $(EXES) pv.o *.out

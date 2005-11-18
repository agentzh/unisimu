RM_F = perl -MExtUtils::Command -e rm_f
THRVIZ = Debugger/thrviz.pl
THRCGI = thrcgi.pl

all: plate.out plate.ast prime.out prime.ast

.SUFFIXES: .pl .ast .tmp .out

.tmp.out:
	perl $< > $@ 2>&1

.pl.tmp:
	perl $(THRVIZ) $<

.pl.ast:
	perl $(THRVIZ) $<

clean:
	$(RM_F) *.tmp *.out *.ast

PROG = plate

run_$(PROG): all
	cd Debugger
	perl $(THRCGI) -a ../$(PROG).ast -r ../$(PROG).out
	cd ..

PROG = prime

run_$(PROG): all
	cd Debugger
	perl $(THRCGI) -a ../$(PROG).ast -r ../$(PROG).out
	cd ..

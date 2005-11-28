RM_F = perl -MExtUtils::Command -e rm_f
THRVIZ = Debugger/thrviz.pl
THRCGI = thrcgi.pl

all: plate.ast plate.out prime.ast prime.out RMP.ast RMP.out

.SUFFIXES: .pl .ast .tmp .out

.tmp.out:
	perl $< > $@ 2>&1

.pl.tmp:
	perl $(THRVIZ) $<

.pl.ast:
	perl $(THRVIZ) $<

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

PROG = RMP

run_$(PROG): all
	cd Debugger
	perl $(THRCGI) -a ../$(PROG).ast -r ../$(PROG).out
	cd ..

test: all ~RMP_out RMP_out run_test

RMP_out:
	perl RMP.pl > $@

run_test:
	diff RMP_out ~RMP_out

clean:
	$(RM_F) *.tmp *.out *.ast RMP_out

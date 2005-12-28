RM_F = perl -MExtUtils::Command -e rm_f
THRVIZ = Debugger/thrviz.pl
THRCGI = thrcgi.pl

all: bigplate.ast bigplate.out plate.ast plate.out prime.ast prime.out RMP.ast RMP.out RMP2.ast RMP2.out

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

PROG = RMP2

run_$(PROG): all
	cd Debugger
	perl $(THRCGI) -a ../$(PROG).ast -r ../$(PROG).out
	cd ..

PROG = bigplate

run_$(PROG): all
	cd Debugger
	perl $(THRCGI) -a ../$(PROG).ast -r ../$(PROG).out
	cd ..

test: all ~RMP_out RMP_out RMP2_out run_test

RMP_out:
	perl RMP.pl > $@

RMP2_out:
	perl RMP2.pl > $@

run_test:
	diff RMP_out ~RMP_out
	diff RMP2_out ~RMP_out

clean:
	$(RM_F) *.tmp *.out *.ast RMP_out RMP2_out

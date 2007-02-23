rm_f = perl -MExtUtils::Command -e rm_f
exe_files = ./arith.exe ./kid.exe ./logic.exe
cs_files = $(patsubst %.exe,%.cs,$(exe_files))
CS_COMPILER = csc

all: $(exe_files)

%.exe: %.cs
	$(CS_COMPILER) /debug /nologo $<

%.cs: sample/%.grammar spike_emitter.pm spike_parser.pm
	perl spike.pl -o $@ $<

test: all
	echo -32 | ./arith.exe
	echo 5 | ./arith.exe
	echo '-3.1+2*(5-5.1)' | ./arith.exe
	@echo '(-3.3 expected)'
	./kid.exe < sample/01.kid
	./kid.exe < sample/02.kid
	./kid.exe < sample/08sample.kid
	./kid.exe < sample/09sample.kid
	echo 'T and T and (F or T)' | ./logic.exe
	echo 'T and T and (F or F)' | ./logic.exe

clean:
	$(rm_f) $(exe_files) $(cs_files)

doc: doc/report.html

%.html: %.pod
	podhtm -s perl.css -o $@ $<

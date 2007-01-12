SHELL = cmd

rm_f = perl -MExtUtils::Command -e rm_f
exe_files = arith.exe kid.exe logic.exe spike2.exe
cs_files = $(patsubst %.exe,%.cs,$(exe_files))

all: $(exe_files)

spike2.exe: spike2.cs util.cs
	csc /debug /nologo $< util.cs

%.exe: %.cs
	csc /debug /nologo $<

%.cs: sample/%.grammar spike_emitter.pm spike_parser.pm
	perl spike.pl -o $@ $<

test: all
	echo -32 | arith
	echo 5 | arith
	echo -3.1+2*(5-5.1) | arith
	@echo (-3.3 expected)
	kid < sample/01.kid
	kid < sample/02.kid
	kid < sample/08sample.kid
	kid < sample/09sample.kid
	echo T and T and (F or T) | logic 
	echo T and T and (F or F) | logic

clean:
	$(rm_f) $(exe_files) $(cs_files)

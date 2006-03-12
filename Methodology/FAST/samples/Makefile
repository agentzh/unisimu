SHELL = cmd
RM_F = perl -MExtUtils::Command -e rm_f

.PHONY: all clean

all: samples.html

%.podx: %.podx.tt
	tpage $< > $@

%.pod: %.podx
	podx $<

%.html: %.pod
	podhtm -s Active.css $<
	$(RM_F) *.tmp

clean:
	$(RM_F) *.png *.tmp *.pod *.podx *.c *.asm *.html

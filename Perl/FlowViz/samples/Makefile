RM_F = perl -MExtUtils::Command -e rm_f
FLOWASM = perl ..\flowasm.pl
FLOWVIZ = perl ..\flowviz.pl
PNG_FILES = test.png test2.png test3.png test4.png test5.png \
	    01test.png 02test.png 03test.png

all: $(PNG_FILES)

.SUFFIXES: .png .fv .fa

.fa.png:
	$(FLOWASM) $<

.fv.png:
	$(FLOWVIZ) $<

clean:
	$(RM_F) $(PNG_FILES) 0*.fa

test: all
	diff test.png ~test.png
	diff test2.png ~test2.png
	diff test3.png ~test3.png
	diff test4.png ~test4.png
	diff test5.png ~test5.png
	diff 01test.png ~01test.png
	diff 02test.png ~02test.png
	diff 03test.png ~03test.png

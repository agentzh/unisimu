RM_F = perl -MExtUtils::Command -e rm_f
FLOWVIZ = perl ..\flowviz.pl
PNG_FILES = test.png test2.png test3.png test4.png

all: $(PNG_FILES)

.SUFFIXES: .png .fv

.fv.png:
	$(FLOWVIZ) $<

clean:
	$(RM_F) $(PNG_FILES)

test: all
	diff test.png ~test.png
	diff test2.png ~test2.png
	diff test3.png ~test3.png
	diff test4.png ~test4.png

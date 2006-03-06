RM_F = perl -MExtUtils::Command -e rm_f

.PHONY: all test clean

all: binary_tree_set3.exe palindrome.exe char_stat.exe

%.exe: %.d
	dmd -quiet $<

test: all
	prove *.t

clean:
	$(RM_F) *.exe *.obj *.map

RM_F = perl -MExtUtils::Command -e rm_f

all: binary_tree_set3.exe palindrome.exe

%.exe: %.d
	dmd -quiet $<

.PHONY: test clean

test: all
	prove *.t

clean:
	$(RM_F) *.exe *.obj *.map

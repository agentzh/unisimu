RM_F = perl -MExtUtils::Command -e rm_f

all: binary_tree_set3.exe

%.exe: %.d
	dmd -quiet $<

.PHONY: test clean

test: all
	prove *.t

clean:
	$(RM_F) binary_tree_set3.exe binary_tree_set3.obj binary_tree_set3.map

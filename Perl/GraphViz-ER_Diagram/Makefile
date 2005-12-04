RM_F = perl -MExtUtils::Command -e rm_f
PNG_FILES = 01test.png 01test2.png 01test3.png 02test.png

all: $(PNG_FILES)

01test.png: 01test.cm
	perl er-diag.pl 01test.cm

01test2.png: 01test.cm
	perl er-diag.pl -t -o $@ 01test.cm

01test3.png: 01test.cm
	perl er-diag.pl -r -o $@ 01test.cm

02test.png: 02test.cm
	perl er-diag.pl 02test.cm

clean:
	$(RM_F) $(PNG_FILES)

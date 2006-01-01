RM_F = perl -MExtUtils::Command -e rm_f
PNG_FILES = 01test.png 01test2.png 01test3.png 02test.png 02test2.png 02test3.png 03test.png \
	news.png course.png

all: $(PNG_FILES)

01test.png: 01test.cm
	perl er-diag.pl 01test.cm

01test2.png: 01test.cm
	perl er-diag.pl -t -o $@ 01test.cm

01test3.png: 01test.cm
	perl er-diag.pl -r -o $@ 01test.cm

02test.png: 02test.cm
	perl er-diag.pl 02test.cm

02test2.png: 02test.cm
	perl er-diag.pl -t -o $@ 02test.cm

02test3.png: 02test.cm
	perl er-diag.pl -r -o $@ 02test.cm

03test.png: 03test.cm
	perl er-diag.pl -s 4x4 03test.cm

news.png: news.cm
	perl er-diag.pl news.cm

course.png: course.cm
	perl er-diag.pl course.cm

clean:
	$(RM_F) $(PNG_FILES) er-diag.bat

install:
	pl2bat er-diag.pl
	copy er-diag.bat E:\perl\bin\

test: all run_test

run_test:
	diff 01test.png ~01test.png
	diff 01test2.png ~01test2.png
	diff 01test3.png ~01test3.png
	diff 02test.png ~02test.png
	diff 02test2.png ~02test2.png
	diff 02test3.png ~02test3.png
	diff 03test.png ~03test.png
	diff news.png ~news.png
	diff course.png ~course.png

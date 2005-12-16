RM_F = perl -MExtUtils::Command -e rm_f

PNG_FILES = 01test.png 02test.png 03test.png 04test.png 05test.png

test: $(PNG_FILES) run_test

.SUFFIXES: .png .rv

.rv.png:
	perl reasonviz.pl -d 150x150 -c $<

run_test:
	diff 01test.png ~01test.png
	diff 02test.png ~02test.png
	diff 03test.png ~03test.png
	diff 04test.png ~04test.png

clean:
	$(RM_F) $(PNG_FILES)

RM_F = perl -MExtUtils::Command -e rm_f
THRVIZ = Debugger\thrviz.pl
THRCGI = thrcgi.pl

all: plate.pl.ast out

out: ~plate.pl
	perl ~plate.pl > $@ 2>&1

plate.pl.ast: plate.pl
	perl $(THRVIZ) plate.pl > ~plate.pl

~plate.pl: plate.pl
	perl $(THRVIZ) plate.pl > $@

clean:
	$(RM_F) plate.pl.ast out ~plate.pl

run_svr: all
	cd Debugger
	perl $(THRCGI) -a ..\plate.pl.ast -r ..\out
	cd ..

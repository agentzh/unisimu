RM_F = perl -MExtUtils::Command -e rm_f
FLAGS = -u zwy -p zwy

.SUFFIXES: .podt .podx .pod .html

.podt.podx:
	perl prepro.pl -u zwy -p zwy $< > $@

.podx.pod:
	podxc $<

.pod.html:
	podhtm -s Active.css $<
	$(RM_F) *.tmp

all:
	perl eval-sql.pl -u zwy -p zwy $(FLAGS) schema.sql
	perl csv2db.pl -u zwy -p zwy $(FLAGS) S.csv P.csv J.csv SPJ.csv

doc: exp1.html exp2.html

clean:
	$(RM_F) *.pod *.podx *.html

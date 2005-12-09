RM_F = perl -MExtUtils::Command -e rm_f
FLAGS = -u zwy -p zwy

all:
	perl eval-sql.pl -u zwy -p zwy $(FLAGS) schema.sql
	perl csv2db.pl -u zwy -p zwy $(FLAGS) S.csv P.csv J.csv SPJ.csv

doc: exp1.podx exp1.pod exp1.html exp2.podx exp2.pod exp2.html \
	exp3.podx exp3.pod exp3.html exp4.podx exp4.pod exp4.html

.SUFFIXES: .podt .podx .pod .html

.podt.podx:
	perl prepro.pl -u zwy -p zwy $< > $@

.podx.pod:
	podxc $<

.pod.html:
	podhtm -s Active.css $<
	$(RM_F) *.tmp

clean:
	$(RM_F) *.pod *.podx NIL

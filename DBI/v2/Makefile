RM_F = perl -MExtUtils::Command -e rm_f
FLAGS = -u zwy -p zwy

all:
	perl eval-sql.pl -u zwy -p zwy $(FLAGS) schema.sql
	perl csv2db.pl -u zwy -p zwy $(FLAGS) S.csv P.csv J.csv SPJ.csv

doc: exp1.pod exp1.html \
	 exp2.pod exp2.html \
	 exp4.pod exp4.html \
	 prelude.pod prelude.html er-diagram.png \
	 exp3.pod exp3.html

.SUFFIXES: .podx .pod .html

.podx.pod:
	podx $<

.pod.html:
	podhtm -s Active.css $<

er-diagram.png: er-diagram.cm
	er-diag -s 4x4 er-diagram.cm

clean:
	$(RM_F) *.tmp *.pod NIL

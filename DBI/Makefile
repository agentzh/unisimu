RM_F = perl -MExtUtils::Command -e rm_f

all : S.sql P.sql J.sql SPJ.sql

S.sql : S-sql-gen.tt S.csv
	tpage S-sql-gen.tt > $@

P.sql : P-sql-gen.tt P.csv
	tpage P-sql-gen.tt > $@

J.sql : J-sql-gen.tt J.csv
	tpage J-sql-gen.tt > $@

SPJ.sql : SPJ-sql-gen.tt SPJ.csv
	tpage SPJ-sql-gen.tt > $@

clean :
	$(RM_F) db/* *.sql

eval-sql : all
	set DSN=DBI:CSV:f_dir=./db
	perl eval-sql.pl *.sql
	set DSN=DBI:ADO:Provider=Microsoft.Jet.OLEDB.4.0;Data Source=test.mdb
	perl eval-sql.pl *.sql
	set DSN=DBI:ODBC:test
	perl eval-sql.pl *.sql

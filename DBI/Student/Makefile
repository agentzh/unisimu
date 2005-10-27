RM_F = perl -MExtUtils::Command -e rm_f
SQL_FILES = clean.sql Student.sql Course.sql SC.sql

all : $(SQL_FILES)

Student.sql : Student-sql-gen.tt Student.csv
	tpage Student-sql-gen.tt > $@

Course.sql : Course-sql-gen.tt Course.csv
	tpage Course-sql-gen.tt > $@

SC.sql : SC-sql-gen.tt SC.csv
	tpage SC-sql-gen.tt > $@

clean :
	$(RM_F) db/* Student.sql Course.sql SC.sql

eval-sql : all
	rem set DSN=DBI:CSV:f_dir=./db
	rem perl eval-sql.pl $(SQL_FILES)
	set DSN=DBI:ADO:Provider=Microsoft.Jet.OLEDB.4.0;Data Source=test.mdb
	perl eval-sql.pl $(SQL_FILES)
	set DSN=DBI:ODBC:test
	perl eval-sql.pl $(SQL_FILES)

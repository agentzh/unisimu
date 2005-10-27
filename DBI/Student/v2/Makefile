RM_F = perl -MExtUtils::Command -e rm_f
SQL_FILES = schema.sql insert.sql
CSV_FILES = Student.csv Course.csv SC.csv

all : $(SQL_FILES)

insert.sql : insert.sql.tt $(CSV_FILES)
	tpage insert.sql.tt > $@

insert.sql.tt : insert-gen.pl insert.sql.tt.tt schema.sql
	perl insert-gen.pl schema.sql > $@

clean :
	$(RM_F) insert.sql insert.sql.tt

eval-sql : all
	rem set DSN=DBI:CSV:f_dir=./db
	rem perl eval-sql.pl $(SQL_FILES)
	set DSN=DBI:ADO:Provider=Microsoft.Jet.OLEDB.4.0;Data Source=test.mdb
	perl eval-sql.pl $(SQL_FILES)
	set DSN=DBI:ODBC:test
	perl eval-sql.pl $(SQL_FILES)

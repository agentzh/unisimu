FLAGS = -u zwy -p zwy

all:
	perl eval-sql.pl $(FLAGS) schema.sql
	perl csv2db.pl $(FLAGS) S.csv P.csv J.csv SPJ.csv

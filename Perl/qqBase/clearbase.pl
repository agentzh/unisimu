use DBI;

my $dsn = $ENV{DSN};
die "error: No env DSN set.\n" unless $dsn;

my $dbh = DBI->connect($dsn, { PrintError => 1, RaiseError => 0 });
$dbh->do("drop table msgs");
$dbh->do("drop table users");
$dbh->disconnect;

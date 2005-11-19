#: insert-data.pl
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-12 2005-11-12

use strict;
use warnings;
use DBI;
use Getopt::Std;

my %opts;
getopts('u:p:', \%opts);

my ($user, $password) = ($opts{u}, $opts{p});

my $csvfile = shift ||
    die "No CSV file given.\n";

my $table;
if ($csvfile =~ /(\w+)\.\w+$/) {
    $table = $1;
} else {
    die "Invalid file name.\n";
}

my $dsn = $ENV{DSN};
die "No env DSN set.\n" unless $dsn;

my $dbh;
if ($user) {
	$dbh = DBI->connect($dsn, $user, $password, { PrintError => 1 });
} else {
	$dbh = DBI->connect($dsn, { PrintError => 1 });
}

open my $in, $csvfile or
    die "error: Can't open $csvfile: $!\n";
$_ = <$in>;
my @flds = split /,/, $_;
my $nflds = @flds;
@place_holders = map { '?' } @flds;

my $sql = "insert into $table (@flds) values (".join(',',@place_holders).')';
warn "sql: $sql\n";
my $sth = $dbh->prepare($sql);

warn "info: Inserting data to $table...\n";
while (<$in>) {
    chomp;
    @flds = split /\s*,\s*/, $_;
    $" = ':';
    #print "*@flds*\n";
    $sth->execute(@flds);
}

$dbh->disconnect();

#: insert-data.pl
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-12 2005-11-12

use strict;
use warnings;
use DBI;

my ($user, $password) = qw(zwy zwy);

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

my $dbh = DBI->connect($dsn, $user, $password, { PrintError => 1 });

open my $in, $csvfile or
    die "error: Can't open $csvfile: $!\n";
$_ = <$in>;
my @flds = split /,/, $_;
my $nflds = @flds;
@flds = map { '?' } @flds;

my $sql = "insert into $table values (".join(',',@flds).')';
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

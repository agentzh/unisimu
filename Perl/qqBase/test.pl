use strict;
use warnings;
use DBI;
use Data::Dumper;

my $dsn = $ENV{DSN};
die "No env DSN set.\n" unless $dsn;

my $dbh = DBI->connect($dsn, { PrintError => 1, RaiseError => 0 });

while (<>) {
    last if /^\s*q\s*$/i;
    next if /^\s*$/;
    my $sth = $dbh->prepare($_);
    $sth->execute;
    my $ref = $sth->fetchrow_arrayref;
    print Data::Dumper->Dump([$ref], [qw(ref)]);
}

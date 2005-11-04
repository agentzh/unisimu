#: session.t

use strict;
use warnings;
use DBI;

my $dsn = $ENV{DSN};
my $where_clause = shift || '';
die "No env DSN set.\n" unless $dsn;

my $dbh = DBI->connect($dsn, { PrintError => 1, RaiseError => 0 });

my $sth = $dbh->prepare("select * from msgs $where_clause order by session_id, offset asc");
$sth->execute;
my $session;
while (my $ref = $sth->fetchrow_hashref) {
    if (!defined $session) {
        $session = $ref->{session_id};
    } elsif ($ref->{session_id} != $session) {
        $session = $ref->{session_id};
        print "-----------------------\n\n";
    }
    my $tmstr = localtime($ref->{msg_time});
    print "$tmstr ", $ref->{msg_from}, "\n";
    print $ref->{msg_body}, "\n\n";
}

$sth = $dbh->prepare("select max(offset) from msgs $where_clause group by session_id order by max(offset)");
$sth->execute;
print "\nSession Length List:\n";
while (my $ref = $sth->fetchrow_arrayref) {
    print $ref->[0]+1, " ";
}
print "\n";
$dbh->disconnect;

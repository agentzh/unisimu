#: test.pl
#: use DBD::CSV to access CSV database
#: 2005-09-16 2005-09-17

use strict;
use warnings;

use DBI;

my $dbh = DBI->connect("DBI:CSV:f_dir=./db", { PrintError => 1 });

foreach (qw(S P J SPJ)) {
    my $sqls = slurp("$_.sql");

    $dbh->{PrintError} = 0;
    $dbh->do("drop table $_");
    $dbh->{PrintError} = 1;

    foreach my $sql (split(/\n\n/s, $sqls)) {
        next if $sql =~ m/^\s*$/s;
        $dbh->do($sql);
    }
}
$dbh->disconnect();

sub slurp {
    my $fname = shift;
    open my $fh, $fname or
        die "Can't open $fname for reading: $!";
    local $/;
    my $content = <$fh>;
    close $fh;
    return $content;
}

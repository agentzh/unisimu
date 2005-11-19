#: eval-sql.pl
#: Eval SQL file using environment 'DSN' as the dsn
#: 2005-10-09 2005-10-19

use strict;
use warnings;
use DBI;
use Getopt::Std;

my %opts;
getopts('u:p:', \%opts);

my ($user, $password) = ($opts{u}, $opts{p});

my @files = map glob, @ARGV;
die "Usage: eval-sql <sql-file1> <sql-file2> ...\n" unless @files;

my $dsn = $ENV{DSN};
die "No env DSN set.\n" unless $dsn;

my $dbh;
if ($user) {
	$dbh = DBI->connect($dsn, $user, $password, { PrintError => 1 });
} else {
	$dbh = DBI->connect($dsn, { PrintError => 1 });
}

my $count = 0;
for (@files) {
    my $sqls = slurp($_);
    foreach my $sql (split(/\n\n/s, $sqls)) {
        next if $sql =~ m/^\s*$/s;
        if (not $dbh->do($sql)) {
			last;
		}
		$count++;
    }
}
print "eval-sql: For tatol $count SQL statements evaluated.\n";
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

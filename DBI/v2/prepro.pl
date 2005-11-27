# TODO:
#   Generalize this
#   and modulize this

use strict;
use warnings;
use File::Spec;

use DBI;
use Getopt::Std;

my %opts;
getopts('u:p:', \%opts);

my @files = map glob, @ARGV or die "Usage: prepro <pod-file>\n";

my ($user, $password) = ($opts{u}, $opts{p});

my $dsn = $ENV{DSN};
die "No env DSN set.\n" unless $dsn;

my $dbh;
if ($user) {
	$dbh = DBI->connect($dsn, $user, $password, { PrintError => 1 });
} else {
	$dbh = DBI->connect($dsn, { PrintError => 1 });
}

my $tmpdir = File::Spec->tmpdir;
my $tmpfile = "$tmpdir/prepro.tmp";

foreach (@files) {
    process_file($_);
}

sub process_file {
    my $infile = shift;

    open my $in, $infile or
        die "Can't open $infile for reading: $!\n";

    my $in_shell = 0;
    my $in_sql = 0;
    my $sql = '';
    while (<$in>) {
        if (not $in_shell and /^=begin\s+shell\s*$/) {
            $in_shell = 1;
        }
        elsif ($in_shell and /^=end\s+shell\s*$/) {
            $in_shell = 0;
        }
        elsif (/^=shell\s+(.+)/) {
            process_cmd($1);
        }
        elsif ($in_shell) {
            process_cmd($_);
        }
        elsif (not $in_sql and /^=begin\s+SQL\s*$/) {
            $in_sql = 1;
        }
        elsif ($in_sql and /^=end\s+SQL\s*$/) {
            process_sql($sql);
            $sql = '';
            $in_sql = 0;
        }
        elsif (/^=SQL\s+(.+)/) {
            process_sql($1);
        }
        elsif ($in_sql) {
            $sql .= $_;
        }
        else {
            print;
        }
    }
    close $in;
}

sub process_cmd {
    my $cmd = shift;
    chomp $cmd;
    $cmd =~ s/^\s+|\s+$//g;
    return if not $cmd;
    print "    \$ $cmd\n";
    #warn $cmd;
    system("$cmd > $tmpfile 2>&1");
    open my $tmp, $tmpfile or
        die "Can't open $tmpfile for reading: $!";
    while (my $line = <$tmp>) {
        print "    $line";
    }
    close $tmp;
    print "\n";
}

sub process_sql {
    my $sql = shift;
    $sql =~ s/^[\s\n]+|[\s\n]+$//sg;
    #warn "---------------\n";
    #warn $sql, "\n";
    #warn "+++++++++++++++\n\n";
    my $dbh = $dbh->do($sql);
    while (my $
}

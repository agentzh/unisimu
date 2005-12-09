#: prepro.pl
#: POD preprocessor for =SQL and =shell
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-26 2005-12-08

# TODO:
#   Generalize this
#   and modulize this

use strict;
use warnings;
use File::Spec;

use DBI;
use Getopt::Std;
use Scalar::Util qw/looks_like_number/;

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
    if ($cmd =~ m/>\s*[^"'>]+\s*(?:2>\&1)?\s*$/) {
        system ($cmd);
    } else {
        system("$cmd > $tmpfile 2>&1");
        open my $tmp, $tmpfile or
            die "Can't open $tmpfile for reading: $!";
        while (my $line = <$tmp>) {
            print "    $line";
        }
        close $tmp;
    }
    print "\n";
}

sub process_sql {
    my $sql = shift;
    $sql =~ s/^[\s\n]+|[\s\n]+$//sg;
    #warn "---------------\n";
    #warn $sql, "\n";
    #warn "+++++++++++++++\n\n";
    my @lines = split "\n", $sql;
    map { $_ = "    $_" if $_ } @lines;
    print join("\n", @lines),"\n\n";
    my $sth = $dbh->prepare($sql);
    my $rv = $sth->execute() or print "$DBI::errstr\n\n";
    if (!$sth->{'NUM_OF_FIELDS'}) { # not a select statement
		local $^W=0;
        if (not defined $rv) {
    		$rv = "undefined number of";
        } elsif ($rv == -1) {
    		$rv = "unknown number of";
        }
		print "[$rv row",
            (looks_like_number($rv) && $rv==1 ? "" : "s"),
            " affected]\n";
        return;
    }
    print "\n=begin html\n\n";
    print qq/<pre>        <table border=1>\n/;
    my @flds = @{ $sth->{NAME} };
    my $firstTime = 1;
    my $row = $sth->fetchrow_arrayref();
    if (!$row) {
        print "\n<tr><td>&nbsp;&nbsp;&nbsp;&nbsp;</td></tr>\n";
    } else {
        while (1) {
            last if not $row;
            if ($firstTime) {
                print qq/\n<tr style="border-top:2px;border-bottom:2px">\n/,
                      join('', map { "<td><font color=blue><B>$_</B></color></td>" } @flds), "\n</tr>\n";
                $firstTime = 0;
            }
            my @data = @$row;
            $row = $sth->fetchrow_arrayref();
            if ($row) {
                print "<tr>\n";
            } else {
                print qq[<tr style="border-bottom:2px">\n];
            }
            map { print "<td>$_</td>\n" } @data;
            print "</tr>\n";
            last if not $row;
        }
    }
    print "</table>\n</pre>\n\n=end html\n\n";
}

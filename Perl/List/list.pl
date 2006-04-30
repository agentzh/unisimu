#: list.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-18 2006-04-30

use strict;
use warnings;
use Getopt::Std;

my %opts;
getopts('re:', \%opts);

my $excl = $opts{e};
#warn $opts{e};
if (defined $excl) {
    $excl =~ s/\./\\./g;
    $excl =~ s/\*/.*/g;
    $excl =~ s/\?/./g;
}
#warn $excl;

my $dir = shift or
    die "Usage: list [-r] [-e exclude-pattern] <input-dir>\n";
#warn $dir;
$dir =~ s/\\/\//g;
process_dir($dir);

sub process_dir {
    my $dname = shift;
    opendir my $dh, $dname or
        die "Can't open directory $dname for reading: $!\n";
    while (my $e = readdir $dh) {
        next if $e eq '.' or $e eq '..';
        $e = "$dname/$e";
        if (-f $e and (!defined $excl or $e !~ /^$excl$/)) {
            my $path = $e;
            $path =~ s/^.\///;
            #warn $dname;
            print "$path\n";
        } elsif (-d $e) {
            if ($opts{r}) {
                process_dir($e);
            }
        }
    }
    closedir $dh;
}

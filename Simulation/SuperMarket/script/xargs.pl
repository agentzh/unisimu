use strict;
use warnings;

my @args;
while (<STDIN>) {
    chomp;
    next if /^\s*$/;
    s/^\s+|\s+$//g;
    push @args, $_;
}
system(@ARGV, @args);

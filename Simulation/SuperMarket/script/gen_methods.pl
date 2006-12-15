use strict;
use warnings;

use List::MoreUtils 'any';
use File::Slurp;

@ARGV || die "Usage: $0 <infile> > <outfile>\n";
my $text = read_file(shift);
my @methods = grep { $_ } split /\s*\n\s*/, $text;
my %methods;
for my $method (@methods) {
    $methods{$method} = 1;
}

open my $in, 'tmon.out' or
    die "Can't open tmon.out for reading: $!\n";

my (@stack, @depth);
<$in>;
while (my $method = <$in>) {
    chomp $method;
    my $depth = 0;
    if ($method =~ s/^\s+//) {
        $depth = length($&);
    }
    if (!@depth) {
        push @depth, $depth;
        push @stack, $method;
    }
    elsif ($depth < $depth[-1]) {
        pop @stack;
        pop @depth;
        while ($depth < $depth[-1]) {
            pop @stack;
            pop @depth;
        }
        if (@depth and $depth == $depth[-1]) {
            $stack[-1] = $method;
        }
    }
    elsif ($depth > $depth[-1]) {
        push @stack, $method;
        push @depth, $depth;
    }
    else {
        $stack[-1] = $method;
    }
    process_method($method);
}

sub process_method {
    my $method = shift;
    if (any { $_ eq $method } @methods) {
        warn "$method matched!\n";
        for (@stack) {
            warn "  adding $_...\n";
            $methods{$_} = 1;
        }
    }
}

print join("\n", sort keys %methods), "\n";

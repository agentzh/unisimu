package CLIPSx::Compiler;

use strict;
use warnings;

use base 'CLIPSx';

#warn "Hi!!!";

package main;

use strict;
use warnings;

our $count;
our @facts;
our $rel_type;
our $module;

our (%prefix, %infix, %infix_prefix, %infix_circumfix, %infix_circum_close);

%infix = (
    '\='  => "test (neq ",
    '=='  => "test (eq ",
    ':='  => "bind ",
);

sub match_prefix {
    my @keys = sort { -($a cmp $b) } keys %::prefix;
    return match($_[0], \@keys);
}

sub match_infix {
    my @keys = sort { -($a cmp $b) } keys %::infix;
    return match($_[0], \@keys);
}

sub match_infix_prefix {
    my @keys = sort { -($a cmp $b) } keys %::infix_prefix;
    return match($_[0], \@keys);
}

sub match_infix_circum_open {
    my @keys = sort { -($a cmp $b) } keys %::infix_circumfix;
    #warn "infix_circum_open: @keys\n";
    return match($_[0], \@keys);
}

sub match_infix_circum_close {
    my $open = pop @_;
    my $close = $::infix_circum_close{$open};
    #warn "infix_circum_close: $close\n";
    return match($_[0], [$close]);
}

sub match {
    $_[0] =~ s/^\s+//;
    my $rkeys = pop;
    for my $key (@$rkeys) {
        #warn "$key => ", $::prefix{$key}, "\n";
        my $len = length($key);
        if (substr($_[0], 0, $len) eq $key) {
            #warn "!!! matched operator \"$key\"\n";
            $_[0] = substr($_[0], $len);
            return $key;
        }
    }
    return undef;
}

sub process_include {
    my ($fname, $linno) = @_;
    my $src = read_file($fname);
    my $saved_infile = $::infile;
    local $::infile = $fname;

    my ($base) = ($fname =~ /([\w-]+)\.\w+$/);
    $base = "f$base" if $base !~ /^[A-Za-z_]/;
    local $::base = $base;

    local $::count = 0;

    my $parser = CLIPSx->new;
    my $data = $parser->program($src);
    if (!defined $data) {
        die "error: $saved_infile (line $linno): can't include file $fname.\n";
    }
    #warn "$data!!!";
    $data;
}

1;

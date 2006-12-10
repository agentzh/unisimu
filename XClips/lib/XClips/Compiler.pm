package XClips::Compiler;

use strict;
use warnings;
use File::Spec;

use base 'XClips::Compiler::Base';

#warn "Hi!!!";

package main;

use strict;
use warnings;

our $count;
our @facts;
our $rel_type;
our $module;
our @Include = '.';
our %Include;

our (%prefix, %infix, %infix_prefix, %infix_circumfix, %infix_circum_close,
     %circumfix, %circum_close);
our (@prefix, @infix, @infix_prefix, @infix_circumfix, @circumfix);

%prefix = (
    '~'  => 'not ',
);

%infix = (
    '%'   => 'mod ',
    '/'   => '/ ',
    'div' => 'div ',
    '-'   => '- ',
    '+'   => '+ ',
    '\='  => 'test (neq ',
    '=='  => 'test (eq ',
    ':='  => 'bind ',
    '>='  => '>= ',
    '>'   => '> ',
    '<='  => '<= ',
    '<'   => "< ",
);

@infix = sort { -($a cmp $b) } keys %infix;

sub match_prefix {
    my @keys = @prefix;
    return match($_[0], \@keys);
}

sub match_infix {
    my @keys = @infix;
    #warn "MATCH INFIX: @keys\n";
    return match($_[0], \@keys);
}

sub match_infix_prefix {
    my @keys = @infix_prefix;
    return match($_[0], \@keys);
}

sub match_circum_open {
    my @keys = @circumfix;
    #warn "circum_open: @keys\n";
    return match($_[0], \@keys);
}

sub match_infix_circum_open {
    my @keys = @infix_circumfix;
    #warn "infix_circum_open: @keys\n";
    return match($_[0], \@keys);
}

sub match_infix_circum_close {
    my $open = pop @_;
    my $close = $::infix_circum_close{$open};
    #warn "infix_circum_close: $close\n";
    return match($_[0], [$close]);
}

sub match_circum_close {
    my $open = pop @_;
    my $close = $::circum_close{$open};
    #warn "circum_close: $close\n";
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
    my $done;
    for my $dir (@Include) {
        my $file = File::Spec->catfile($dir, $fname);
        if (-f $file) {
            $fname = $file;
            $done = 1;
            last;
        }
    }
    if (!$done) {
        die "error: $::infile (line $linno): Can't find include file $fname ",
            "in \@Include.\n\t(\@Include contains: @Include)\n";
    }

    my $path = File::Spec->rel2abs($fname);
    return "" if $Include{$path};
    $Include{$path} = 1;
    #warn "including file $fname...";
    my $src = read_file($fname);
    my $saved_infile = $::infile;
    local $::infile = $fname;

    my ($base) = ($fname =~ /([\w-]+)\.\w+$/);
    $base = "f$base" if $base !~ /^[A-Za-z_]/;
    local $::base = $base;

    local $::count = 0;

    my $parser = XClips::Compiler->new;
    my $data = $parser->program($src);
    if (!defined $data) {
        die "error: $saved_infile (line $linno): can't include file $fname.\n";
    }
    #warn "$data!!!";
    $data;
}

1;

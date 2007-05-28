#!/usr/bin/env perl

use strict;
use warnings;

#use Smart::Comments;
use threads;
use threads::shared;
use PV;

my $in;

my @buffer: shared = ();
my $K = 10;

my $R_i = 0;
my $M_i = 0;
my $P_i = 0;

semas(
    R_enter => $K,
    M_enter => 0,
    P_enter => 0,
);

async { #-# R
    while (1) {
        P('R_enter');
        my $s = read_data();
        warn "R: Reading [$s]...\n" if defined $s;

        threads->yield;

        $buffer[$R_i] = $s;
        $R_i = ($R_i + 1) % $K;
        V('M_enter');
        last if not defined $s;
    }
    #-#
}

#sleep(3);

async { #-# M
    while (1) {
        P('M_enter');
        my $s = $buffer[$M_i];
        my $new;
        if (defined $s) {
            $new = lc($s);
            $new = ucfirst($new);
            $new =~ s/ {2,}/ /g;
            $new =~ s/[^A-Za-z\s,.]/./g;
            warn "M: Converting [$s] to [$new]...\n";
        }

        threads->yield;

        $buffer[$M_i] = $new;
        $M_i = ($M_i + 1) % $K;
        V('P_enter');
        last if not defined $s;
    }
    #-#
}

#sleep(3);

async { #-# P
    while (1) {
        P('P_enter');
        my $s = $buffer[$P_i];
        ### P (s): $s
        warn "P: Printing [$s]...\n" if defined $s;
        if (defined $s) { print "$s\n"; }
        else { last; }
        $P_i = ($P_i + 1) % $K;
        V('R_enter');
    }
    #-#
}

my $thr;
foreach $thr (threads->list) {
    if ($thr->tid && !threads::equal($thr, threads->self)) {
        $thr->join;
    }
}

sub read_data {
    if (not $in) {
        my $file = 'lcc.data.txt';
        open $in, $file or
            die "Can't open $file for reading: $!\n";
    }
    my $s = <$in>;
    chomp $s if $s;
    my $retval = defined $s && $s ne '' ? $s : undef;
    ### $retval
    $retval
}


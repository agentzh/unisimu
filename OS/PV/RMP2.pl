#: RMP2.pl
#: R-M-P problem for 2 buffers
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-21 2005-11-28

use strict;
use warnings;

use threads;
use threads::shared;
use PV;

my $in;

my @buf_A: shared = ();
my @buf_B: shared = ();
my $K = 5;

my $R_i_A = 0;
my $M_i_A = 0;
my $M_i_B = 0;
my $P_i_B = 0;

semas(
	R_enter_A => $K,
	M_enter_A => 0,
    M_enter_B => $K,
	P_enter_B => 0,
);

async { #-# R
	while (1) {
		P('R_enter_A');
		my $s = read_data();
		warn "R: Reading $s...\n" if defined $s;
		$buf_A[$R_i_A] = $s;
		$R_i_A = ($R_i_A + 1) % $K;
		V('M_enter_A');
		last if not defined $s;
	}
    #-#
}

async { #-# M
    while (1) {
        P('M_enter_A');
        my $s = $buf_A[$M_i_A];
        my $new = chr($s + ord('A')) if defined $s;
		warn "M: Converting $s to $new...\n" if defined $s;
        $M_i_A = ($M_i_A + 1) % $K;
        V('R_enter_A');

        P('M_enter_B');
        $buf_B[$M_i_B] = $new;
        $M_i_B = ($M_i_B + 1) % $K;
        V('P_enter_B');
        last if not defined $s;
    }
    #-#
}

async { #-# P
    while (1) {
        P('P_enter_B');
        my $s = $buf_B[$P_i_B];
		warn "P: Printing '$s'...\n" if defined $s;
        V('M_enter_B');
        if (defined $s) { print "$s\n"; }
        else { last; }
        $P_i_B = ($P_i_B + 1) % $K;
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
        my $file = -f '../RMP_in' ? '../RMP_in' : 'RMP_in';
        open $in, $file or
            die "Can't open $file for reading: $!\n";
    }
    my $s = <$in>;
    chomp $s if $s;
    return $s;
}

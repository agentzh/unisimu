#: RMP.pl
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-21 2005-11-28

use strict;
use warnings;

use threads;
use threads::shared;
use Thread::Semaphore;

my $in;
my %semas;

my @buffer: shared = ();
my $K = 5;

my $R_i = 0;
my $M_i = 0;
my $P_i = 0;

new_semaphore(
	R_enter => $K,
	M_enter => 0,
	P_enter => 0,
);

async { #-# R
	while (1) {
		P('R_enter');
		my $s = read_data();
		warn "R: Reading $s...\n" if defined $s;
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
        my $new = chr($s + ord('A')) if defined $s;
		warn "M: Converting $s to $new...\n" if defined $s;
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
		warn "P: Printing '$s'...\n" if defined $s;
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
        my $file = -f '../RMP_in' ? '../RMP_in' : 'RMP_in';
        open $in, $file or
            die "Can't open $file for reading: $!\n";
    }
    my $s = <$in>;
    chomp $s if $s;
    return $s;
}

sub new_semaphore {
	my %names = @_;
	foreach (keys %names) {
        #warn "$_ => $names{$_}\n";
        $semas{$_} = Thread::Semaphore->new($names{$_});
	}
}

sub P {
    #sleep(1);
    my $name = shift;
    $semas{$name}->down;
    threads->yield;
}

sub V {
    #sleep(1);
    my $name = shift;
    $semas{$name}->up;
    threads->yield;
}

# bigplate.pl

use strict;
use warnings;

use threads;
use threads::shared;
use PV;

use constant {
    TIMES => 15,
    PLATE_SZ => 10,
};

$| = 1;

my @plate : shared := ();

# apple stack pointer (stack grows downwards)
my $apple_sp : shared := 0;

# orange stack pointer (stack grows upwards)
my $orange_sp : shared := PLATE_SZ - 1;

semas(
    can_put        => PLATE_SZ,
    can_get_apple  => 0,
    can_get_orange => 0,
    apple_sp => 1,
    orange_sp => 1,
);

$PV::sleep = 0.03;

async { #-# father
    for (1..TIMES) {
        P('can_put');
        P('apple_sp');
        print "Father is putting an apple...\n";
        $plate[$apple_sp] = 'apple';
        $apple_sp++;
        V('apple_sp');
        V('can_get_apple');
    }
    #-#
}

async { #-# mother
    for (1..TIMES) {
        P('can_put');
        P('orange_sp');
        print "Mother is putting an orange...\n";
        $plate[$orange_sp] = 'orange';
        $orange_sp--;
        V('orange_sp');
        V('can_get_orange');
    }
    #-#
}

async { #-# sun
    for (1..TIMES) {
        P('can_get_orange');
        P('orange_sp');
        my $fruit = $plate[++$orange_sp];
        print "  Sun got an $fruit!\n";
        V('orange_sp');
        V('can_put');
    }
    #-#
}

async { #-# daughter
    for (1..TIMES) {
        P('can_get_apple');
        P('apple_sp');
        my $fruit = $plate[--$apple_sp];
        print "  Daughter got an $fruit!\n";
        V('apple_sp');
        V('can_put');
    }
    #-#
}

my $thr;
foreach $thr (threads->list) { 
    if ($thr->tid && !threads::equal($thr, threads->self)) { 
        $thr->join; 
    } 
}

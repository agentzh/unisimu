#: plate.pl
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-14 2005-11-14

use strict;
use warnings;

use threads;
use threads::shared;
use PV;

$| = 1;
my $max = 5;

my $plate : shared = 0;

semas(
    can_put        => 1,
    can_get_apple  => 0,
    can_get_orange => 0,
);

async { #-# father
    for (1..$max) {
        P('can_put');
        print "Father is putting an apple...\n";
        die if $plate;
        $plate = 'apple';
        V('can_get_apple');
    }
    #-#
}

async { #-# mother
    for (1..$max) {
        P('can_put');
        print "Mother is putting an orange...\n";
        die if $plate;
        $plate = 'orange';
        V('can_get_orange');
    }
    #-#
}

async { #-# sun
    for (1..$max) {
        P('can_get_orange');
        die if !$plate;
        print "  Sun got an $plate!\n";
        $plate = '';
        V('can_put');
    }
    #-#
}

async { #-# daughter
    for (1..$max) {
        P('can_get_apple');
        die if !$plate;
        print "  Daughter got an $plate!\n";
        $plate = '';
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

#: LR0_DFA_Pre.pm
#: Preliminary AST for DFAs of sets of LR(0) items Class
#: Copyright (c) 2006 Agent Zhang
#: 2006-06-20 2006-06-20

package LR0::DFA::Pre;

use strict;
use warnings;

use LR0_Item;
use LR0_Edge;
use LR0_ItemSet;

sub build {
    my ($class, $ast) = @_;
    my $pre_dfa = {};
    $pre_dfa;
}

1;

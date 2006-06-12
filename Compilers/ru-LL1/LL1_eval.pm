# LL1_eval.pm
# Evaluator for LL(1) grammar
# 2006-06-12 2006-06-12

package LL1::Eval;

use strict;
use warnings;

use LL1_table;
use LL1_runtime;

sub eval {
    my ($self, $ast, $input) = @_;
    my $Firsts = LL1::Table::first_sets($ast);
    my $Follows = LL1::Table::follow_sets($ast, $Firsts);
    my $table = LL1::Table::LL1_table($ast, $Firsts, $Follows);
    $X::str = $input;
    $X::pos = 0;
    LL1::Runtime::eval_table($table, $ast->{startrule});

}

sub error { $LL1::Runtime::Error }

sub offset { $X::pos }

1;

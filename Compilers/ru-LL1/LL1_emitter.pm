#: LL1_emitter.pm
#: code emitter for ru::LL1

package LL1::Emitter;

use strict;
use warnings;

use LL1_table;
use Data::Dumper;

sub emit {
    my ($self, $ast, $filetype, $package) = @_;
    my $Firsts = LL1::Table::first_sets($ast);
    my $Follows = LL1::Table::follow_sets($ast, $Firsts);
    my $table = LL1::Table::LL1_table($ast, $Firsts, $Follows);
    my $s = Data::Dumper->Dump([$table, $X::tokens], ["${package}::table", "X::tokens"]);
    warn $s;
}

1;

__DATA__

package main;

our $LL1_TRACE = undef;   # default off

package X;

our ($str, $pos, $level);

package [% package %];

use strict;
use warnings;


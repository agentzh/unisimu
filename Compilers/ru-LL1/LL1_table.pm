#: LL1_table.pm
#: generating First/Follow sets and LL(1) parsing table
#: Copyright (c) 2006 Agent Zhang
#: 2006-06-06 2006-06-07

package LL1::Table;

use strict;
use warnings;
#use Data::Dumper::Simple;

use LL1;
use Set::Scalar;
use Carp 'croak';

our $Trace;

# inserts an elem to the set and returns the 
#   size increasement of the resulting set
sub set_insert ($$) {
    my ($set, $elem) = @_;
    my $size = $set->size;
    $set->insert($elem);
    $set->size - $size;
}

# adds the second set to the first, returns
#   the size increasement of the result
sub set_add ($$) {
    my ($A, $B) = @_;
    my $size = $A->size;
    $A->insert($B->elements);
    $A->size - $size;
}

# compute the First sets for all the nonterminals
#   in the grammar AST
sub first_sets ($) {
    my $ast = $_[0];
    my %Firsts;
    my $rules = $ast->{rules};
    for my $rulename (keys %$rules) {
        $Firsts{$rulename} = Set::Scalar->new;
    }
    my $eps = LL1::eps;
    my $changes = 1;
    my $i = 0;
    while ($changes) {
        $changes = 0;
        while (my ($rulename, $choices) = each %$rules) {
            my $fset = $Firsts{$rulename};
            for my $production (@$choices) {
                my $continue = 1;
                for my $item (@$production) {
                    #print "trying $item for $rulename";
                    last if !$continue;
                    if ($item =~ /^\W/) {
                        # item is a terminal
                        $changes += set_insert($fset, $item);
                        $continue = 0;
                        last;
                    }
                    my $temp = $Firsts{$item};
                    #warn "!!! $temp";
                    #warn "!!! ", $temp->contains($eps);
                    croak "Nonterminal '$item' not defined in grammar\n"
                        if !defined $temp;
                    if ($temp->contains($eps)) {
                        $temp = $temp->clone;
                        $temp->delete($eps);
                    } else {
                        $continue = 0;
                    }
                    $changes += set_add($fset, $temp);
                }
                if ($continue) {
                    $changes += set_insert($fset, $eps);
                }
            }
            warn "  $rulename: $fset\n" if $Trace;
        }
        if ($Trace) {
            $i++;
            warn "Pass $i: $changes elems added to First sets\n";
        }
        #warn Dumper(%Firsts);
    }
    \%Firsts;
}

# calculate the First set of a string of terminals and/or nonterminals
sub string_first_set ($@) {
    my $Firsts = shift;
    my @symbols = @_;
    my $fset = Set::Scalar->new;
    my $eps = LL1::eps;
    my $continue = 1;
    for my $symbol (@symbols) {
        last if !$continue;
        if ($symbol =~ /^\W/o) {
            # it is a terminal
            $fset->insert($symbol);
            $continue = 0;
            last;
        }
        my $temp = $Firsts->{$symbol};
        croak "Nonterminal $symbol not found in the given First sets"
            if !$temp;
        if ($temp->contains($eps)) {
            $temp = $temp->clone;
            $temp->delete($eps);
        } else {
            $continue = 0;
        }
        $fset->insert($temp->elements);
    }
    if ($continue) {
        $fset->insert($eps);
    }
    $fset;
}

# compute the Follow sets for all the nonterminals
#   in the grammar AST
sub follow_sets ($$) {
    my ($ast, $Firsts) = @_;
    my %Follows;
    my $startrule = $ast->{startrule};
    my $rules = $ast->{rules};
    for my $rulename (keys %$rules) {
        $Follows{$rulename} = Set::Scalar->new;
    }
    $Follows{$startrule}->insert(LL1::eof);
    my $eps = LL1::eps;
    my @addto;
    while (my ($rulename, $choices) = each %$rules) {
        for my $production (@$choices) {
            my @items = @$production;
            for my $i (0..$#items) {
                my $item = $items[$i];
                if ($item =~ /^\W/o) {
                    # it is a terminal
                    next;
                }
                my $tail_first_set =
                    string_first_set($Firsts, @items[$i+1..$#items]);
                if ($tail_first_set->contains($eps)) {
                    $tail_first_set->delete($eps);
                    if ($rulename ne $item) {
                        if ($Trace) {
                            warn "  Add Follow($rulename) to Follow($item)\n";
                        }
                        push @addto, [ $rulename, $item ];
                    }
                }
                my $item_follow_set = $Follows{$item};
                if (!defined $item_follow_set) {
                    die "error: Nonderminal $item not defined in the grammar.\n";
                }
                $item_follow_set->insert($tail_first_set->elements);
            }
        }
    }
    if ($Trace) {
        for my $rulename (sort keys %Follows) {
            warn "  $rulename: $Follows{$rulename}\n";
        }
        warn "Pass 1 completed for computing Follow sets.\n";
    }
    my $i = 1;
    my $changes = 1;
    while ($changes) {
        $changes = 0;
        for my $pair (@addto) {
            my ($src, $dest) = @$pair;
            $changes += set_add($Follows{$dest}, $Follows{$src});
        }
        if ($Trace) {
            $i++;
            warn "Pass $i: $changes elems added to the Follow sets.\n";
        }
    }
    \%Follows;
}

# construct an LL(1) parsing table from the grammar AST,
#   First sets, and Follow sets
sub LL1_table ($$$) {
    my ($ast, $Firsts, $Follows) = @_;
    my $Table = {};
    my ($eof, $eps) = (LL1::eof, LL1::eps);
    my $rules = $ast->{rules};
    while (my ($rulename, $choices) = each %$rules) {
        for my $production (@$choices) {
            my @items = @$production;
            if (@items == 1 and $items[0] eq $eps) {
                @items = ();
            }
            my $temp = string_first_set($Firsts, @items);
            my $continue = 0;
            for my $token ($temp->elements) {
                if ($token eq $eps) {
                    $continue = 1;
                    next;
                }
                add_production(
                    $Table,
                    $rulename, $token, \@items,
                    1  # replace existing entry if there's any
                );
            }
            next if !$continue;
            for my $elem ($Follows->{$rulename}->elements) {
                add_production(
                    $Table,
                    $rulename, $elem, \@items,
                    undef  # don't replace exising entry if there's any
                );
            }
        }
    }
    $Table;
}

# add the given production choice to the LL(1) parsing table
sub add_production ($$$$$) {
    my ($Table, $N, $T, $production, $replace) = @_;
    $Table->{$N} ||= {};
    my $entry = $Table->{$N}->{$T};
    if ($entry) {
        warn "warning: Duplicate entries found in LL(1) parsing table,\n";
        if ($replace) {
            warn "  replacing [ $N -> @$entry ] with [ $N -> @$production ]\n";
            $Table->{$N}->{$T} = $production;
        } else {
            warn "  discarding [ $N -> @$production ]\n";
        }
    } else {
        $Table->{$N}->{$T} = $production;
    }
}

1;

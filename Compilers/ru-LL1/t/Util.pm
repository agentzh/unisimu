# t/Util.pm

package t::Util;

use strict;
use warnings;

#use Data::Dumper::Simple;
use base 'Exporter';

our @EXPORT_OK = qw(
    parse_grammar
    dump_fsets
);

sub parse_grammar {
    my $grammar = shift;
    $grammar =~ s/^[\n\s]+|[\n\s]+$//gso;
    my @rules = split /\s*\n+\s*/, $grammar;
    my ($startrule, %rules);
    for (@rules) {
        if (/^(\w+)\s*:\s*(.*)/) {
            my ($rulename, $choices) = ($1, $2);
            $startrule = $rulename if !$startrule;
            my @list = split /\s*\|\s*/, $choices ;
            #warn Dumper(@list);
            if (@list) {
                map { $_ = [ split /\s+/, $_ ]; } @list;
            } else {
                @list = [];
            }
            if ($choices =~ /\|\s*$/) {
                push @list, [];
            }
            $rules{$rulename} = \@list;
        } else {
            die "Syntax error in grammar: $_";
        }
    }
    { startrule => $startrule, rules => \%rules };
}

# dump out First/Follow sets returned by subs first_sets, follow_sets, and etc.
sub dump_fsets {
    my $fsets = $_[0];
    my $out = '';
    for my $symbol (sort keys %$fsets) {
        my $set = $fsets->{$symbol};
        my @elems = $set->elements;
        map { if ($_ eq '/\Z/') { $_ = '$'; } } @elems;
        @elems = sort @elems;
        $out .= "$symbol: @elems\n";
    }
    $out;
}

1;

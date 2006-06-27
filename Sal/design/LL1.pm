#LL1.pm written for generating LL1 parsing table
#which was expressed throught a hash
#version 0.0.6
#2006-06-23 2006-6-24

package LL1;

use strict;
use warnings;
use Ffset;

my $set = Ffset->new();


my @prod;
sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = {};
	
	bless ($self, $class);
	return $self;
}

sub get_prod {
	shift;
	@prod = @_;
	$set->get_prod(@prod);
}

sub set_sstate {
	shift;
	my $stat = shift;
	$set->set_sstate($stat);
}

sub build_table {
	shift;
	my %LL1_table;
	for my $prod (@prod) {
		#print "$prod\n";
		my($left, $right) = split /\s+\->\s+/, $prod;
		my @fset = $set->get_first($right);
		if($set->has_epsilon(@fset)) {
			for($set->get_follow($left)) {
				warn "one more recoder in row $left column $right\n" if $LL1_table{$left}->{$_};
				$LL1_table{$left}->{$_} = $right;
			}
		} else {
			for(@fset) {
				warn "one more recoder in row $left column $right\n" if $LL1_table{$left}->{$_};
				$LL1_table{$left}->{$_} = $right;
			}
		}
	}
	return \%LL1_table;
}


1;
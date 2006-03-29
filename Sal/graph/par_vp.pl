#parsing file to obtain vector parametric eqution
#2006-3-29
#Copyright(c): Sal Zhong 
#2006-3-29

use strict;
use warnings;
use Data::Dumper;
use drawgraph2;

my($file, $t) = @ARGV;

my @equations;
my (%vec_ep, %coe_ep, %points);
my $cnt = 0;
my $state = 'N';

open my $src, $file or
	die "cannot open $file: $!\n";
while(<$src>) {
	next if /^\s*$/;
	chomp;
	if($state eq 'N') {
		if(/item\d/) {
			$cnt++;
			$state = 'v';
		} else {
			synerror($.);
		}
	} elsif($state eq 'v') {
		if(/equation/) {
			$state = 'vc';
		} else {
			synerror($.);
		}
	} elsif($state eq 'vc') {
		if(/^\s+(\w) = (.*)$/) {
			$vec_ep{$1} = $2;
			#print $2, "\n";
			
		}else {
			$state = 'c';
			redo;
		}
	} elsif($state eq 'c') {
		if(/coefficient/) {
			$state = 'cc';
		} else {
			synerror($.);
		}
	} elsif($state eq 'cc'){
		if(/^\s+([abc][xy]) = (.*)$/) {
			$coe_ep{$1} = $2;			
		} else {
			$state = 'p';
			redo;
		}
	} elsif($state eq 'p') {
		if(/points/) {
			$state = 'pc';
		} else {
			synerror($.);
		}
	} elsif($state eq 'pc') {
		if(/\s+\((x[012]),\s*(y[012])\) = \((\d+),\s*(\d*)\)/) {
			$points{$1} = $3;
			$points{$2} = $4;			
		} else {
			if(/END/) {
				push @equations, [\%vec_ep, \%coe_ep, \%points];
			} else {
				$state = 'N';
				redo;
			}
		}
	} else {
		die "sth abnormal happen!\n";
	}
}
close $src or
	die "cannot close $file: $!\n";

for(@equations) {
	substring($_->[1], $_->[2]);
	substring($_->[0], $_->[1]); 
	#die;
	#gen_img($_->[0], $t);
	animate($_->[0], $t);
	
}

#warn Data::Dumper::Dumper($_->[0]);

 
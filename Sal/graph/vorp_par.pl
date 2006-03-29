#parsing file to obtain vector or polar eqution
#2006-3-29
#Copyright(c): Sal Zhong 
#2006-3-29

use strict;
use warnings;
use Data::Dumper;
use drawgraph2;

my($file, $t) = @ARGV;
die "no input file or type\n" unless $file && $t;

my @equations;
my (%equation, %coe_eq);
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
			$state = 'e';
		} else {
			synerror($.);
		}
	} elsif($state eq 'e') {
		if(/equation/) {
			$state = 'ec';
		} else {
			synerror($.);
		}
	} elsif($state eq 'ec') {
		if(/^\s+(\w) = (.*)$/) {
			$equation{$1} = $2;
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
		if(/^\s+(\w+) = (.*)$/) {
			$coe_eq{$1} = $2;			
		} else {
			if(/END/) {
				push @equations, [\%equation, \%coe_eq];
				last;
			}else {
				$state = 'N';
				push @equations, [\%equation, \%coe_eq];
				redo;			
			}
		}
	} else {
		die "sth abnormal happen!\n";
	}
}




 for(@equations) {
	substring($_->[1], $_->[2]);
	substring($_->[0], $_->[1]); 
	#die;
	#gen_img($_->[0], $t);
	animate($_->[0], $t);	
}

#warn Data::Dumper::Dumper(@equations);
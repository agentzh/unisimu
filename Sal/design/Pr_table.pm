#get_protable.pl parses p.txt
#to get a hash of priority
#version 0.0.1
#2006-06-25 2006-6-25
package Pr_table;
require Exporter;
@ISA = qw/Exporter/;
@EXPORT = qw/gen_table/;


use strict;
use warnings;
use Data::Dumper;

#print Data::Dumper::Dumper(gen_table());
sub gen_table {
	my $file = 'p2.txt' || shift;
	my @op;
	my %priority;
	open my $f, $file or die "cannot open $file to read!: $!\n";

	while(<$f>) {		
		next if /^\s*$/;	
		s/^\s+//;	
		if($. == 1) {		
			@op = split  /\s+/;
		} else {
			my @records = split /\s+/;
			my $i = 1;
			for(@op) {
				$priority{$records[0]}->{$_} = $records[$i];
				$i++;
			}
		}
	}
	close $f or die $!;
	return \%priority;
}

1;
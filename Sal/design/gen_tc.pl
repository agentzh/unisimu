#gen_tc.pl generates tmporary code for input string
#provided by user under a certain sytax grammar
#version 0.0.1
#2006-06-25 2006-06-25

use strict;
use warnings;
use LL1;
use Data::Dumper;
use Pr_table;
use Test::More tests => 6;

my @prods = (
					'E	-> T E@',
					'E@ -> + T E@',
					'E@	-> - T E@',
					'E@ -> @',
					'T	-> F T@',
					'T@	-> * F T@',
					'T@	-> / F T@',
					'T@ -> @',
					'F  -> p F@', #this contex free grammer isn't suitable for LL(1), how can i adapt it?
					'F@ -> ^ F',
					'F@ -> @',					
					'p	-> ( E )',
					'p	-> i',

				);
my $LL = LL1->new();
$LL->get_prod(@prods);
#$LL->set_sstate('stat');
my $table = $LL->build_table();
my @string1 = ('( ( ( ( ( ( i + i * ( i + i / i ) * i ) + i ) ) ) ) ) )',
			  #'( i + i )',
			  #'i + i * i / i - i',
			  #'i / i - i * i + i ^ i',
			  );
my @string2 = ('( ( ( ( ( ( i + i * ( i + i / i ) * i ) + i ) ) ) )',
			  'i + i * i / i - ',
			  'i + i * * i',
			  );

my(@input, @stack, @operand, @operator);
my $priority = gen_table();

for my $input (@string1) {
	@input = reverse split /\s+/, $input;
	unshift @input, '#';
	@stack = ('#', 'E');	

	parse();
}


sub parse {
	my $i = 0;
	@operator = ('#');
	@operand = ();
	while(1) {
		my ($last_index1, $last_index2) = ($#input, $#stack);
		if($stack[$last_index2] eq '#' && $input[$last_index1] eq '#') {
			
			gen_tc('#');
			print "Accept!!!\n";

			return 1;
			last;
		} else {
			if($stack[$last_index2] eq $input[$last_index1]) {
				if($input[$last_index1] eq 'i') {
					push @operand, 'i';
				} else {
					#push @operator, $input[$last_index1];
					gen_tc($input[$last_index1]);
				}
				
				pop @stack;
				pop @input;
				
			} else {
				my $subkey = $input[$last_index1];
				$subkey = '@' unless $subkey =~ m/\S/;
				#print $subkey, "\n";
				if(defined (my $prod = $$table{$stack[$last_index2]}->{$subkey}) ) {
					pop @stack;
					my @prod = split /\s+/, $prod;
					for(my $i = @prod-1; $i >= 0; $i--) {
						push @stack, $prod[$i] unless $prod[$i] eq '@';
					}
				} else {
					print "grammar error!\n";
					last;
				}
			}
		}
		#print $i++, ": @stack		@input \n";
	}
}

sub gen_tc {
	
	my $op = shift;
	#print $op, "  ", $operator[$#operator], "\n";
	my $comp = $priority->{$op}{$operator[$#operator]};
	if($comp eq '<') {
		my ($opnd1, $opnd2) = (pop @operand, pop @operand);
		print "( $operator[$#operator], $opnd1, $opnd2, T),\n";
		push @operand, 'T';
		pop @operator;
		gen_tc($op);
		
		
	} elsif($comp eq '=') {
		pop @operator;
	} else {
		#print "h\n";
		push @operator, $op;
	}
}
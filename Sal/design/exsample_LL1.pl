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
use lex2;

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
					'p	-> number',
					'p  -> identifier',

				);
my $LL = LL1->new();
$LL->get_prod(@prods);
#$LL->set_sstate('stat');
my $table = $LL->build_table();
my @string1 = get_words('tiny3');

my(@input, @stack, @operand, @operator);
my $priority = gen_table();


@input = reverse  @string1;
unshift @input, ['#', '#'];
@stack = ('#', 'E');	
my $t = 1;
parse();



sub parse {
	my $i = 0;
	@operator = ('#');
	@operand = ();
	while(1) {
		my ($last_index1, $last_index2) = ($#input, $#stack);
		if($stack[$last_index2] eq '#' && $input[$last_index1][0] eq '#') {
			
			gen_tc('#');
			print "Accept!!!\n";

			return 1;
			last;
		} else {
			if($stack[$last_index2] eq $input[$last_index1][1]) {
				if($input[$last_index1][1] eq 'number' || $input[$last_index1][1] eq 'identifier') {
					push @operand, $input[$last_index1][0];
				} else {
					#push @operator, $input[$last_index1];
					gen_tc($input[$last_index1][0]);
				}
				
				pop @stack;
				pop @input;
				
			} else {
				my $subkey = $input[$last_index1][1];
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
		my $T = "T$t";
		print "( $operator[$#operator], $opnd1, $opnd2, $T),\n";
		push @operand, $T;
		$t++;
		pop @operator;
		gen_tc($op);
		
		
	} elsif($comp eq '=') {
		pop @operator;
	} else {
		#print "h\n";
		push @operator, $op;
	}
}
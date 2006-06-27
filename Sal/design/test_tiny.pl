#gen_tc.pl generates tmporary code for input string
#provided by user under a certain syntax grammar
#version 0.0.1
#2006-06-25 2006-06-25

use strict;
use warnings;
use LL1;
use Data::Dumper;
use Pr_table;
#use Test::More tests => 6;
use lex;

my @prods = (
				'program	    -> stmt-sequence',
				'stmt-sequence  -> statement stmt-sequence@',
				'stmt-sequence@ -> ; statement stmt-sequence@',
				'stmt-sequence@ -> @',
				'statement		-> if-stmt',
				'statement		-> repeat-stmt',
				'statement		-> assign-stmt',
				'statement		-> read-stmt',
				'statement		-> write-stmt',
				#'statement		-> @',
				'if-stmt		-> if exp then stmt-sequence if-stmt@',
				'if-stmt@		-> end',
				'if-stmt@		-> else stmt-sequence end',
				#'if-stmt		-> if exp then stmt-sequence end',
				#'if-stmt		-> if exp then stmt-sequence else stmt-sequence end',
				'repeat-stmt	-> repeat stmt-sequence until exp',
				'assign-stmt	-> identifier := exp',
				'read-stmt		-> read identifier',
				'write-stmt		-> write exp',
				#'exp			-> simple-exp comparison-op simple-exp',
				#'exp			-> simple-exp',
				'exp			-> simple-exp exp@',
				'exp@			-> comparison-op simple-exp exp@',
				'exp@			-> @',
				'comparison-op	-> <',
				'comparison-op	-> =',
				'simple-exp		-> term simple-exp@',
				'simple-exp@	-> addop term simple-exp@',
				'simple-exp@	-> @',
				#'simple-exp		-> simple-exp addop term',
				#'simple-exp		-> term',
				'addop			-> +',
				'addop			-> -',
				'term			-> factor term@',
				'term@			-> mulop factor term@',
				'term@			-> @',
				'mulop			-> *',
				'mulop			-> /',
				'factor			-> ( exp )',
				'factor			-> number',
				'factor			-> identifier',
);


my $LL = LL1->new();
$LL->get_prod(@prods);
$LL->set_sstate('program');
my $table = $LL->build_table();
#print Data::Dumper::Dumper($table);

my $file = shift || 'tiny';
my @string1 = get_words($file);
#print @string1 , "\n";
my(@input, @stack, @operand, @operator);
my $priority = gen_table();


@input = reverse  @string1;
unshift @input, ['#', '#'];
@stack = ('#', 'program');	
my $t = 1;
parse();



sub parse {
	#print "enter!\n";
	print "step 2 - syntax parsing for $file\n";
	my $i = 0;
	@operator = ('#');
	@operand = ();
	
	while(1) {
		print "\n-------------------------------------------------------------------\n\n";
		print $i++, "\n\nParsing Stack: ", join " ",@stack,	"\n\n";
		print "Input:         ";
		for(my $g=@input-1;$g>=0;$g--) {
			print $input[$g]->[0], " ";
		}
		print "\n\n";

		my ($last_index1, $last_index2) = ($#input, $#stack);
		#print $stack[$last_index2], "\t", $input[$last_index1][0], "\n";
		if($stack[$last_index2] eq '#' && $input[$last_index1][0] eq '#') {
			
			
			#gen_tc('#');
			print "Action:        Syntax Ok!!!\n";

			return 1;
			last;
		} else {
			print "Action:        ";
			if($stack[$last_index2] eq $input[$last_index1][1]) {
				print "match\n";
				if($input[$last_index1][1] eq 'number' || $input[$last_index1][1] eq 'identifier') {
					push @operand, $input[$last_index1][0];
				} else {
					#push @operator, $input[$last_index1];
					#gen_tc($input[$last_index1][0]);
				}
				
				pop @stack;
				pop @input;
				
			} else {
				my $subkey = $input[$last_index1][1];
				$subkey = '@' unless $subkey =~ m/\S/;
				#print $subkey, "\n";
				if(defined (my $prod = $$table{$stack[$last_index2]}->{$subkey}) ) {
					
					print pop @stack, " -> $prod\n";
					my @prod = split /\s+/, $prod;
					for(my $i = @prod-1; $i >= 0; $i--) {
						push @stack, $prod[$i] unless $prod[$i] eq '@';
					}
				} else {
					print "syntax error! near \"", $input[$last_index1][0],"\"  at line  $input[$last_index1][2] in $file\n";
					last;
				}
			}
		}
		#print $i++, ": @stack	\n";	#@input 
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
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
				'E	-> T E@ #',
				'E@ -> + T E@',
				'E@	-> - T E@',
				'E@ -> @',
				'T	-> F T@',
				'T@	-> * F T@',
				'T@	-> / F T@',
				'T@ -> @',
				#'F	-> p ^ F', #this contex free grammer isn't suitable for LL(1), how can i adapt it?
				'F	-> p',
				'p	-> ( E )',
				'p	-> identifier',

);


my $LL = LL1->new();
$LL->get_prod(@prods);
$LL->set_sstate('E');
my $table = $LL->build_table();
print Data::Dumper::Dumper($table);

my $file = shift || 'tiny5';
my @string1 = get_words($file);
#print @string1 , "\n";
my(@input, @stack, @operand, @operator);
my $priority = gen_table();


@input = reverse  @string1;
for(@input) {
	print $_->[0], "\n";
}
unshift @input, ['#', '#'];
#@stack = ('#', 'program');
my $syntree = {
		'0' => ['E', {}],
};
parse($syntree);

sub parse {
	my $syn = shift;
	for(sort keys %$syn) {
		next if $syn->{$_}[0] eq '@';
		print $syn->{$_}[0], "\n";
		if($syn->{$_}[0] eq '#' && $input[$#input][1] eq '#') {
			warn "Syntax Ok!!!\n";
			return 1;	
		} 
		elsif($syn->{$_}[0] eq $input[$#input][1]) {
			pop @input;
		}
		else {
			
			my $subkey = $input[$#input][1];
			#print $subkey;
			#$subkey = '@' unless =~ m/\S/;
			if(defined (my $prod = $$table{$syn->{$_}[0]}->{$subkey})) {
				my @prod = split /\s+/, $prod;
				for my $i (0..@prod-1) {
						$syn->{$_}[1]->{$i}[0] = $prod[$i];
						$syn->{$_}[1]->{$i}[1] = {};
						
				}
				parse($syn->{$_}[1]);
			} else {
				warn "syntax error! near \"", $input[$#input][0],"\"  at line  $input[$#input][2] in $file\n";
				return;
			}
		}
	}
	
}
			


=pod

sub parse {
	my $i = 0;
	
	while(1) {
		my ($last_index1, $last_index2) = ($#input, $#stack);
		print $stack[$last_index2], "\t", $input[$last_index1][0], "\n";
		if($stack[$last_index2] eq '#' && $input[$last_index1][0] eq '#') {			
			warn "Syntax Ok!!!\n";
			return 1;			
		} else {
			if($stack[$last_index2] eq $input[$last_index1][1]) {
								
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
					warn "syntax error! near \"", $input[$last_index1][0],"\"  at line  $input[$last_index1][2] in $file\n";
					last;
				}
			}
		}
		print $i++, ": @stack	\n";	#@input 
	}
}


=cut
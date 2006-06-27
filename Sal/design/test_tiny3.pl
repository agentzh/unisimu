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
use GraphViz;


my @prods = (	'program	    -> stmt-sequence #',
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
#@stack = ('#', 'program');
my $syntree = {
		'0' => ['program', {}],
};

my %EntityStyle =
(
    shape => 'box',
    style => 'filled',
    fillcolor => '#f5f694',
    #fontname => $font,
);

my %PropStyle =
(
    shape => 'ellipse',
    style => 'filled',
    fillcolor => '#B8EFEE',
    #fontname => $font,
);

my %EdgeStyle =
(
    color => 'red',
);

my %RelationStyle =
(
    shape => 'diamond',
    style => 'filled',
    fillcolor => '#c7f75c',
    #fontname => $font,
);

my %InitArgs = (
    
    ratio => 'auto',
    #no_overlap => 1,
    directed => 0,
    #height => $height,
    #width => $width,
    node => \%EntityStyle,
    edge => \%EdgeStyle,
);

$InitArgs{no_overlap} = 1;
my $img = 'syntree.png';
my $g = GraphViz->new(%InitArgs
					  );
my %edge;
my $t = 1;
print "step 4 - image of syntree for $file generated\n", '-' x 40, "\n\n";
parse($syntree);

print "$img generated...\n" if $g->as_png($img);
#print Data::Dumper::Dumper($syntree);
sub parse {
	my $syn = shift;
	for(sort keys %$syn) {	
		#my $label = $syn->{$_}[0]." ".$t++;
		#$g->add_node($syn->{$_}, 'label' => $syn->{$_}[0]);

		next if $syn->{$_}[0] eq '@';
		#print $syn->{$_}[0], "\n";
		if($syn->{$_}[0] eq '#' && $input[$#input][1] eq '#') {
			warn "Syntax Ok!!!\n";
			return 1;	
		} 
		elsif($syn->{$_}[0] eq $input[$#input][1]) {
			$g->add_node($syn->{$_}, label => $input[$#input][0]);
			#$g->add_edge($syn->{$_} => $input[$#input][0] );
			#$edge{$syn->{$_}[0]}->{$input[$#input][0]} = 1;
			
			pop @input;
		}
		else {
			
			my $subkey = $input[$#input][1];
			#print $subkey;
			#$subkey = '@' unless =~ m/\S/;
			if(defined (my $prod = $$table{$syn->{$_}[0]}->{$subkey})) {
				$g->add_node($syn->{$_}, 'label' => $syn->{$_}[0], fontcolor => '#FF0000');
				my @prod = split /\s+/, $prod;
				for my $i (0..@prod-1) {
						#my $label = $syn->{$_}[0]." $t++";
						
						
						#$edge{$syn->{$_}[0]}->{$prod[$i]} = 1;
						
						$syn->{$_}[1]->{$i}[0] = $prod[$i];
						$syn->{$_}[1]->{$i}[1] = {};
						$g->add_node($syn->{$_}[1]->{$i}, label => $prod[$i]);
						$g->add_edge($syn->{$_} => $syn->{$_}[1]->{$i});
						
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
#a sample using LL1.pm to get 
#a LL1 parser table
#2006-06-23 2006-6-23

use strict;
use warnings;
use LL1;
use Data::Dumper;
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
my @string1 = ('( ( ( ( ( ( i + i * ( i + i / i ) * i ) + i ) ) ) ) )',
			  'i + i * i / i - i',
			  'i / i - i * i + i ^ i',
			  );
my @string2 = ('( ( ( ( ( ( i + i * ( i + i / i ) * i ) + i ) ) ) )',
			  'i + i * i / i - ',
			  'i + i * * i',
			  );

my(@input, @stack);

for my $input (@string1) {
	@input = reverse split /\s+/, $input;
	unshift @input, '#';
	@stack = ('#', 'E');	

	is(parse(), 1, $input);
}

for my $in (@string2) {
	@input = reverse split /\s+/, $in;
	unshift @input, '#';
	@stack = ('#', 'E');
TODO: {	
	is(parse(), 1, $in);
      }
     
 }
sub parse {
	my $i = 0;
	while(1) {
		my ($last_index1, $last_index2) = ($#input, $#stack);
		if($stack[$last_index2] eq '#' && $input[$last_index1] eq '#') {
			#print "Accept!!!\n";
			return 1;
			last;
		} else {
			if($stack[$last_index2] eq $input[$last_index1]) {
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
					#warn "grammar error!\n";
					last;
				}
			}
		}
		#print $i++, ": @stack		@input \n";
	}
}
#print Data::Dumper::Dumper($LL->build_table());

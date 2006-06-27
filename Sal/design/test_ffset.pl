use Ffset;
use strict;
use warnings;

my @prods = (
				'program	    -> stmt-sequence',
				'stmt-sequence  -> statement stmt-sequence@',
				'stmt-sequence@ -> ; statement stmt-sequence',
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
				'factor		-> ( exp )',
				'factor		-> number',
				'factor		-> identifier',
);


my $obj = Ffset->new();
$obj->get_prod(@prods);
$obj->set_sstate('program');
#print "first set: ", join "\n", $obj->get_first('exp'), "\n";
print "follow set: ", join "\n", $obj->get_follow('exp'), "\n";
#print Data::Dumper::Dumper(\%tag);
my @non = ('program','stmt-sequence','stmt-sequence@','statement','statement','if-stmt','if-stmt@','repeat-stmt',
		   'assign-stmt','read-stmt','write-stmt','exp','comparison-op','simple-exp','simple-exp@','addop','term',
		   'mulop','mulop','factor');		
for(@non) {
	print "$_ follow set: ", join "\n", $obj->get_follow($_), "\n";
}

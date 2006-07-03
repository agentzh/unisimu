use GraphViz::Flowchart::Asm;

use strict;
use warnings;

my @fafiles = glob("*.fa");
my $obj = GraphViz::Flowchart::Asm->new();
for(@fafiles) {
	print ".fa file is $_, outputfile: \n";
	my $outfile = <>;
	chomp $outfile;
	open OF, ">$outfile";
	$obj->compile($_);
	my $gv = $obj->graphviz();
	print OF $gv->as_debug();
}

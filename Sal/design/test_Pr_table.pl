use Pr_table;
use Data::Dumper;
gen_table('p2.txt');
print Data::Dumper::Dumper(gen_table());


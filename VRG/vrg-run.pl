use strict;
use warnings;

my $profile = shift;
my $data = `$^X pro.pl -s $profile -g go -t halt -q`;
my $pro_src = "(deffacts vector_facts \"$profile\"\n";
open my $in, '<', \$data;
while (<$in>) {
    next if !/^\S+ \S+ \S+$/;
    chomp;
    $pro_src .= "    ($_)\n";
}
$pro_src .= ")\n";
close $in;
print $pro_src;

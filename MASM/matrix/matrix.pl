use strict;
use warnings;

my $i = hex(10);
for(1..15){
    for(1..16){
        print chr($i++), ' ';
    }
    print "\n";
}

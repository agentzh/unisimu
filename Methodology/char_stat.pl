use strict;
use warnings;
use encoding 'GBK';
use Encode 'decode';

my (%chars, $total);
while (<>){
    $_ = decode('GBK', $_);
    s/[^,£¬¡£.\n ]/$chars{$&}++;$total++/ge;
}
printf "$_: %.3f\n", $chars{$_}/$total for (sort keys %chars);

__END__
# Original version:

while (<>){
    s/[^,.\n ]/$c{$&}++;$t++/ge;
}
printf "$_: %.3f\n", $c{$_}/$t for (sort keys %c);

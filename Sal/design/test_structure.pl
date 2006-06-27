use strict;
use warnings;
use Data::Dumper;

my $syn = {
		'1' => ['E', {}],
};

$syn->{'1'}[1]->{'0'}[0] = 't';
$syn->{'1'}[1]->{'0'}[1] = {};
print Data::Dumper::Dumper($syn);


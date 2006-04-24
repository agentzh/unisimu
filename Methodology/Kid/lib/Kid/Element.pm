package Kid::Element;

use strict;
use warnings;

sub child {
    my $self = shift;
    while (my ($key, $val) = each %$self) {
        next if $key =~ /^__[A-Z]+/;
        #warn "$key - $val";
        return $val;
    }
}

1;

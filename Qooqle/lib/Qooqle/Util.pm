package Qooqle::Util;

use strict;
use warnings;

sub highlight ($$$) {
    my $class = shift;
    my ($content, $keys) = @_;
    my $escaped_content = Jifty->web->escape($content);
    for my $key (@$keys) {
        my $pat = quotemeta Jifty->web->escape($key);
        $escaped_content =~ s{$pat}{<b>$&</b>}g;
    }
    $escaped_content;
}

1;

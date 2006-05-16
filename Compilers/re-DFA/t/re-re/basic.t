# basic.t

use Test::Base;
use re::re;

plan tests => 1 * blocks();

run {
    my $block = shift;
    if (defined $block->re) {
        is re::re::translate($block->re), $block->out, $block->name;
    } elsif ($block->error) {
        ok !defined translate($block->re), $block->name;
    }
}

__DATA__

=== TEST 1:
--- re
(a|b)*(aa|bb)(a|b)*
--- out
(a|b)*(aa|bb)(a|b)*



=== TEST 2:
--- re
abc
--- out
abc



=== TEST 3:
--- re
a|b|c
--- out
a|b|c



=== TEST 4:
--- re
((aa))*
--- out
(aa)*



=== TEST 5;
--- re
ab*c
--- out
ab*c



=== TEST 6:
--- re
a  (b| )*
--- out
a  (b| )*



=== TEST 7:
--- re
(a|)b*
--- out
(a|)b*



=== TEST 8:
--- re:
--- out:



=== TEST 9:
--- re
((()*))*
--- out
(()*)*

package t::Kid_XML;
use Test::Base -Base;
use Kid::XML;

our @EXPORT = qw(run_tests);

*pl = \&Kid::XML::translate;

sub run_tests {
    for my $block (blocks()) {
        if ($block->xml) {
            is pl($block->kid), $block->xml, $block->name;
        } elsif ($block->error) {
            ok !defined pl($block->kid), $block->name;
        }
    }
}

1;
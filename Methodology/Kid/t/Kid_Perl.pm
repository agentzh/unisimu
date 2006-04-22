package t::Kid_Perl;
use Test::Base -Base;
use Kid::Perl;

our @EXPORT = qw(run_tests);

*pl = \&Kid::Perl::translate;

sub run_tests {
    for my $block (blocks()) {
        if ($block->perl) {
            is pl($block->kid), $block->perl, $block->name;
        } elsif ($block->error) {
            ok !defined pl($block->kid), $block->name;
        }
    }
}

1;
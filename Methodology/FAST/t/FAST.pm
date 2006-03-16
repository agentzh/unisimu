#: FAST.pm
#: Test scaffold framework for t/sanity.t, t/basic.t, and etc.
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-16 2006-03-16

package t::FAST;

use Test::Base -Base;
use FAST;

our @EXPORT = qw(run_tests);

sub run_tests () {
    for my $block (blocks()) {
        run_test($block);
    }
}

sub run_test ($) {
    my $block = shift;
    my $src = $block->src;
    my $g = FAST->new(\$src);
    ok $g, 'obj ok - '.$block->name;
    warn FAST->error() if not $g;
    #$g->as_png('tmp1.png');
    is( $g->as_asm, $block->asm ) if defined $block->asm;
    my $ast = $g->structured(optimized => 0);
    is( $ast->as_c, $block->unopt, 'unopt ok - '.$block->name );
    #$ast->as_png('tmp2.png');
    $ast = $g->structured(optimized => 1);
    is( $ast->as_c, $block->opt, 'opt ok - '.$block->name );
    #$ast->as_png('tmp3.png');
}

1;
__END__

=head1 NAME

t::FAST - Test scaffold framework for t/sanity.t, t/basic.t, and etc.

=head1 SYNOPSIS

    use t::FAST;
    plan tests => 3 * blocks();
    run_tests;

    __DATA__

    === TEST 1: elementary
    --- src
    entry => [a]
    [a] => exit
    --- asm
        do a
        exit
    --- unopt
    do L:=1
    while (L>0) {
        if (L=1) {
            do a
            do L:=0
        }
    }
    --- opt
    do a

    === TEST 2: blah blah blah
    ...

=head1 DESCRIPTION

=cut

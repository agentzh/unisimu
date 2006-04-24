#: get_vars.t
#: Test Kid::MathModel::get_vars
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-24 2006-04-24

use strict;
use warnings;

use Test::Base;
use Kid;
use Kid::MathModel;

plan tests => 5 * blocks();

run {
    my $block = $_[0];
    my $name = $block->name;

    my $parser = Kid::Parser->new;
    my $parse_tree = $parser->program( $block->kid );
    ok $parse_tree, "parse tree ok - $name";

    my $vars = Kid::MathModel::get_vars( $parse_tree );
    ok $vars, "retval ok - $name";

    is ref $vars, 'HASH', "retval is a hash ref - $name";
    my @keys = eval_keys($vars);
    my $keys = join(' ', @keys);

    my @expected_keys = sort split( /\s+/, $block->vars );
    my $expected_keys = join(' ', @expected_keys);
    is $keys, $expected_keys, "keys ok - $name";
    is_deeply(
        [ sort map { my $e = $_; map { $_->{__VALUE__} } @$e; } values %$vars ],
        [ @expected_keys ],
        "values ok - $name",
    );
};

sub eval_keys {
    my $vars = shift;
    my @res;
    while (my ($key, $value) = each %$vars) {
        for (1..@$value) {
            #warn "Pushing key $key...";
            push @res, $key;
        }
    }
    sort @res;
}

__DATA__

=== TEST 1
--- kid
x:=5; yy:=z*5-(x2+y3-7/y4*x);
--- vars
x yy z x2 y3 y4 x



=== TEST 2
--- kid
if (x+5-(z)>=32) { if (5=aaa) { x3:=x3+3 } }
--- vars
x z aaa x3 x3



=== TEST 3
--- kid
if (x<x_) { x := x+1 }
y := x -1;
--- vars
x x_ x x y x

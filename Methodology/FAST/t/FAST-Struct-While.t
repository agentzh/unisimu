#: FAST-Struct-While.t
#: Test FAST::Struct::While
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-09 2006-03-11

use strict;
use warnings;

use Test::More tests => 84;
use Test::Differences;
use FAST::Struct::Seq;
#use Data::Dumper::Simple;

BEGIN { use_ok('FAST::Struct::While'); }

my $class = 'FAST::Struct::While';

my $while = $class->new('<p>', '[L:=1]');
ok $while;
isa_ok $while, $class;

# Test $while->entry/$while->exit:
is( $while->entry, $while->head, "`entry' is `head'" );
isnt( $while->entry, $while->condition, "`entry' is not `condition'" );
isnt( $while->exit,  $while->body, "`exit' is not `body'" );
is( $while->exit,  $while->tail, "`exit' is `tail'" );

is( $while->condition->label, '<p>' );
is( $while->body->label, '[L:=1]' );
is( $while->head->label, '' );
is( $while->tail->label, '' );
is( $while->exit->label, '' );

# Test $while->condition:
my $cond = $while->condition;
ok $cond;
isa_ok $cond, 'FAST::Node';
is( $cond->label, '<p>' );

# Test $while->body:
my $body = $while->body;
ok $body;
isa_ok $body, 'FAST::Node';
is( $body->label, '[L:=1]' );

is( $while->as_c, <<'_EOC_' );
while (p) {
    do L:=1
}
_EOC_

is( $while->as_c(1), <<'_EOC_' );
    while (p) {
        do L:=1
    }
_EOC_

is( $while->as_c(2), <<'_EOC_' );
        while (p) {
            do L:=1
        }
_EOC_

# Test ->subs
ok $while->subs('<p>', '<q>');
my @elems = $while->elems;
is( $elems[0]->label, '' );
is( $elems[1]->label, '<q>' );
is( $elems[2]->label, '[L:=1]' );
is( $elems[3]->label, '' );

# Test method `might_pass'
ok $while->might_pass('<q>');
ok !$while->might_pass('<p>');
ok $while->might_pass('[L:=1]');
ok !$while->might_pass('[L:=3]');
ok !$while->might_pass('[L:=2]');

# Test method `must_pass'
ok $while->must_pass('<q>');
ok !$while->must_pass('<p>');
ok !$while->must_pass('[L:=1]');
ok !$while->must_pass('[L:=3]');
ok !$while->must_pass('[L:=2]');

ok $while->subs('[L:=1]', FAST::Struct::Seq->new('[L:=2]', '[L:=3]'));
is( $while->as_c, <<'_EOC_' );
while (q) {
    do L:=2
    do L:=3
}
_EOC_

ok $while->must_pass('<q>');
ok !$while->must_pass('<p>');
ok !$while->must_pass('[L:=1]');
ok !$while->must_pass('[L:=3]');
ok !$while->must_pass('[L:=2]');

my $outfile = 't/05basic.png';
unlink $outfile if not -f $outfile;
$while->as_png($outfile);
ok -f $outfile;

{
    # Test nested FAST::Struct::While:
    my $e = $class->new('<p>', '[L:=1]');
    my $while  = $class->new('<r>', $e);
    is( $while->condition->label, '<r>' );
    is( $while->body, $e );

    is( $while->body->condition->label, '<p>' );
    is( $while->body->body->label, '[L:=1]' );

    is( $while->entry, $while->head, "`entry' is the first node" );
    is( $while->exit,  $while->tail, "`exit' is the last node" );

    eq_or_diff( $while->as_c, <<'_EOC_' );
while (r) {
    while (p) {
        do L:=1
    }
}
_EOC_

    eq_or_diff( $while->as_c(1), <<'_EOC_' );
    while (r) {
        while (p) {
            do L:=1
        }
    }
_EOC_


    eq_or_diff( $while->as_c(2), <<'_EOC_' );
        while (r) {
            while (p) {
                do L:=1
            }
        }
_EOC_

    ok( !$while->must_pass('[L:=1]') );
    ok( !$while->must_pass('[L:=2]') );

    ok( $while->must_pass('<r>') );
    ok( !$while->must_pass('<p>') );

    ok( $while->might_pass('[L:=1]') );
    ok( !$while->might_pass('[L:=2]') );

    ok( $while->might_pass('<r>') );
    ok( $while->might_pass('<p>') );
    ok( !$while->might_pass('<q>') );

    my $saved_while = $while->clone;
    ok $saved_while;
    isa_ok $saved_while, 'FAST::Struct::While';
    isnt( $saved_while->id, $while->id, 'clone works' );

    ok $while->subs('[L:=1]', FAST::Struct::Seq->new('[L:=1]', '[L:=3]'));
    eq_or_diff( $while->as_c, <<'_EOC_' );
while (r) {
    while (p) {
        do L:=1
        do L:=3
    }
}
_EOC_

    eq_or_diff( $saved_while->as_c, <<'_EOC_' );
while (r) {
    while (p) {
        do L:=1
    }
}
_EOC_

    ok ! $while->subs('[L:=8]', FAST::Struct::Seq->new('[L:=1]', '[L:=3]'));
    ok $while->subs('[L:=3]', FAST::Struct::Seq->new('[L:=2]', '[L:=1]'));
    eq_or_diff( $while->as_c, <<'_EOC_' );
while (r) {
    while (p) {
        do L:=1
        do L:=2
        do L:=1
    }
}
_EOC_

    ok( !$while->must_pass('[L:=1]') );
    ok( !$while->must_pass('[L:=2]') );
    ok( !$while->must_pass('[L:=3]') );

    ok( $while->might_pass('[L:=1]') );
    ok( $while->might_pass('[L:=2]') );
    ok( !$while->might_pass('[L:=3]') );
    ok( !$while->might_pass('[L:=4]') );

    ok $while->subs('[L:=1]', FAST::Struct::Seq->new('[L:=3]', '[L:=4]'));
    eq_or_diff( $while->as_c, <<'_EOC_' );
while (r) {
    while (p) {
        do L:=3
        do L:=4
        do L:=2
        do L:=3
        do L:=4
    }
}
_EOC_

    my $e1 = $while->body->body->first->first;
    is( $e1->label, '[L:=3]' );
    my $e2 = $while->body->body->second->second->first;
    is( $e2->label, '[L:=3]' );
    isnt( $e1->id, $e2->id, 'id updated during cloning' );

    my $outfile = 't/06basic.png';
    unlink $outfile if not -f $outfile;
    $while->as_png($outfile);
    ok -f $outfile;

}

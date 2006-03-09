#: FAST-Struct-Seq.t
#: Test FAST::Struct::Seq
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-08 2006-03-09

use strict;
use warnings;

use Test::More tests => 73;
#use Data::Dumper::Simple;

BEGIN { use_ok('FAST::Struct::Seq'); }

my $class = 'FAST::Struct::Seq';

my $seq = $class->new('[f]', '[L:=1]');
ok $seq;
isa_ok $seq, $class;

# Test $seq->entry/$seq->exit:
is( $seq->entry, $seq->first, "`entry' is `first'" );
is( $seq->exit,  $seq->second, "`exit' is `second'" );
is $seq->entry->label, '[f]';
is $seq->exit->label, '[L:=1]';

# Test $seq->first:
my $first = $seq->first;
#warn Dumper($first);
ok $first;
isa_ok $first, 'FAST::Node';
is( $first->label, '[f]' );

# Test $seq->second:
my $second = $seq->second;
ok $second;
isa_ok $second, 'FAST::Node';
is( $second->label, '[L:=1]' );

is $seq->as_c, "do f\ndo L:=1\n";

# Test ->subs and $seq->elems in list context:
ok $seq->subs('[f]', '<p>');
my @elems = $seq->elems;
is( $elems[0]->label, '<p>' );
is( $elems[1]->label, '[L:=1]' );

# Test $seq->elems in scalar context:
my $relems = $seq->elems;
is( $relems->[0]->label, '<p>' );
is( $relems->[1]->label, '[L:=1]' );

is $seq->as_c, "if (p) {\ndo L:=1\n";

$relems->[1] = 'exit';
is( $seq->second, 'exit' );
$relems->[1] = FAST::Node->new('exit');
is( $seq->first->label,  '<p>' );

# Test $seq->entry:
is $seq->entry->label, '<p>';

# Test methods `might_pass' and `must_pass':
ok $seq->might_pass('<p>');
ok !$seq->might_pass('[f]');
ok $seq->must_pass('<p>');
ok !$seq->must_pass('[f]');

{
    # Test embedded FAST::Struct::Seq:
    my $e1 = $class->new('[L:=1]', '[L:=2]');
    my $e2 = $class->new('[L:=3]', '[L:=4]');
    my $s  = $class->new($e1, $e2);
    is( $s->first, $e1 );
    is( $s->second, $e2 );

    is( $s->first->first->label, '[L:=1]' );
    is( $s->first->second->label, '[L:=2]' );
    is( $s->second->first->label, '[L:=3]' );
    is( $s->second->second->label, '[L:=4]' );

    is( $s->entry, $s->first->first, "`entry' is the first node" );
    is( $s->exit,  $s->second->second, "`exit' is the last node" );

    is( $s->first->as_c, "do L:=1\ndo L:=2\n" );
    is( $s->second->as_c, "do L:=3\ndo L:=4\n" );
    is( $s->as_c, "do L:=1\ndo L:=2\ndo L:=3\ndo L:=4\n" );
    is( $s->as_c(1), <<'_EOC_' );
    do L:=1
    do L:=2
    do L:=3
    do L:=4
_EOC_
    is( $s->as_c(2), <<'_EOC_' );
        do L:=1
        do L:=2
        do L:=3
        do L:=4
_EOC_

    ok( $s->must_pass('[L:=1]') );
    ok( $s->first->second->must_pass('[L:=2]') );
    ok( $s->first->must_pass('[L:=2]') );
    ok( $s->must_pass('[L:=2]') );
    ok( $s->must_pass('[L:=3]') );
    ok( $s->must_pass('[L:=4]') );
    ok( not $s->must_pass('[L:=5]') );

    ok( $s->might_pass('[L:=1]') );
    ok( $s->might_pass('[L:=2]') );
    ok( $s->might_pass('[L:=3]') );
    ok( $s->might_pass('[L:=4]') );
    ok( not $s->might_pass('[L:=5]') );

    my $saved_s = $s->clone;
    ok $saved_s;
    isa_ok $saved_s, 'FAST::Struct::Seq';
    isnt( $saved_s->id, $s->id, 'clone works' );
    is( $saved_s->as_c, "do L:=1\ndo L:=2\ndo L:=3\ndo L:=4\n", 'clone works' );

    ok $s->subs('[L:=1]', '[a]');
    is( $s->as_c, "do a\ndo L:=2\ndo L:=3\ndo L:=4\n" );
    is( $saved_s->as_c, "do L:=1\ndo L:=2\ndo L:=3\ndo L:=4\n", 'cloned copy stays intact' );

    ok $s->subs('[L:=3]', '[c]');
    is( $s->as_c, "do a\ndo L:=2\ndo c\ndo L:=4\n" );

    ok $s->subs('[L:=4]', '[d]');
    is( $s->as_c, "do a\ndo L:=2\ndo c\ndo d\n" );

    ok ! $s->subs('[L:=4]', '[e]');
    is( $s->as_c, "do a\ndo L:=2\ndo c\ndo d\n" );

    ok ! $s->subs('[L:=5]', '[e]');
    is( $s->as_c, "do a\ndo L:=2\ndo c\ndo d\n" );

    ok $s->subs('[L:=2]', FAST::Node->new('[b]'));
    is( $s->as_c, "do a\ndo b\ndo c\ndo d\n" );
    is( $saved_s->as_c, "do L:=1\ndo L:=2\ndo L:=3\ndo L:=4\n" );

    ok $s->subs('[b]', $saved_s);
    is( $s->as_c, <<'_EOC_' );
do a
do L:=1
do L:=2
do L:=3
do L:=4
do c
do d
_EOC_

    my $outfile = 't/02basic.png';
    unlink $outfile if not -f $outfile;
    $s->as_png($outfile);
    ok -f $outfile;
}

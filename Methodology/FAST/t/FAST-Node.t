#: FAST-Node.t
#: Test FAST::Node
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-09 2006-03-09

use strict;
use warnings;
use Test::More tests => 48;

BEGIN { use_ok('FAST::Node'); }

my $node = FAST::Node->new('abc');
ok $node;
isa_ok $node, 'FAST::Node';

like( $node->id, qr/^\S+$/ );
is( $node->label, 'abc', 'label ok' );
ok( $node->might_pass('abc'), 'might pass ok' );
ok( ! $node->might_pass('abcd'), 'might pass ok' );
ok( $node->must_pass('abc'), 'must pass ok' );
ok( ! $node->must_pass('ab'), 'must pass ok' );

is( $node->as_c, "abc\n", 'as_c works for single node' );
is( $node->as_c(1), "    abc\n", 'as_c(1)' );
is( $node->as_c(2), "        abc\n", 'as_c(2)' );

my $node2 = FAST::Node->new('abc');
ok $node2;
isa_ok $node2, 'FAST::Node';

like( $node->id, qr/^\S+$/ );
isnt( $node2->id, $node->id, 'nodes with the same labels have different ids' );
is( $node->label, 'abc' );

# empty flux node:
$node = FAST::Node->new;
ok $node;
isa_ok $node, 'FAST::Node';
like( $node->id, qr/^\S+$/ );
is( $node->label, '' );
ok( $node->might_pass(''), 'might pass ok' );
ok( ! $node->might_pass('abc'), 'might pass ok' );
ok( $node->must_pass(''), 'must pass ok' );
ok( ! $node->must_pass('a'), 'must pass ok' );
is( $node->as_c, "", 'as_c works for empty node' );
is( $node->as_c(1), "", 'as_c(1) works for empty node' );
is( $node->as_c(2), "", 'as_c(2) works for empty node' );

# Test as_c for different labels:
$node = FAST::Node->new('[abc]');
ok $node;
isa_ok $node, 'FAST::Node';
is( $node->as_c, "do abc\n" );

$node = FAST::Node->new('[printf("%d", a)]');
ok $node;
isa_ok $node, 'FAST::Node';
is( $node->as_c, "do printf(\"%d\", a)\n" );

$node = FAST::Node->new('<p>');
ok $node;
isa_ok $node, 'FAST::Node';
is( $node->as_c, "if (p) {\n", 'as_c for <p> node' );

$node = FAST::Node->new('<a > 4>');
ok $node;
isa_ok $node, 'FAST::Node';
is( $node->as_c, "if (a > 4) {\n", 'as_c for <a > 4> node' );

my $saved_node = $node->clone;
ok $saved_node;
isa_ok $saved_node, 'FAST::Node';
is( $saved_node->as_c, "if (a > 4) {\n", 'as_c for <a > 4> node' );
isnt( $saved_node->id, $node->id );

$node = FAST::Node->new('[a]');
ok $node;
is( $node->entry, $node, 'the entry of a node is itself' );
is( $node->exit, $node, 'the exit of a node is also itself' );
my $outfile = 't/tmp.png';
unlink $outfile if -f $outfile;
$node->as_png($outfile);
ok -f $outfile;

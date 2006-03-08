#: FAST-Struct-Seq.t
#: Test FAST::Struct::Seq
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-08 2006-03-08

use strict;
use warnings;

use Test::More tests => 23;
#use Data::Dumper::Simple;

BEGIN { use_ok('FAST::Struct::Seq'); }

my $class = 'FAST::Struct::Seq';

my $seq = $class->new('[f]', '[L:=1]');
ok $seq;
isa_ok $seq, $class;

# Test $seq->entry/$seq->exit:
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

# Test $seq->elems in list context:
ok $seq->subs('[f]', '<p>');
my @elems = $seq->elems;
is( $elems[0]->label, '<p>' );
is( $elems[1]->label, '[L:=1]' );

# Test $seq->elems in scalar context:
my $relems = $seq->elems;
is( $relems->[0]->label, '<p>' );
is( $relems->[1]->label, '[L:=1]' );
$relems->[1] = 'exit';
is( $seq->second, 'exit' );
is( $seq->first->label,  '<p>' );

# Test $seq->entry:
is $seq->entry->label, '<p>';

# Test methods `might_pass' and `must_pass':
ok $seq->might_pass('<p>');
ok !$seq->might_pass('[f]');
ok $seq->must_pass('<p>');
ok !$seq->must_pass('[f]');

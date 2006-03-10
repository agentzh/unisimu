#: FAST-Struct-If.t
#: Test FAST::Struct::If
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-09 2006-03-10

use strict;
use warnings;

use Test::More tests => 96;
use Test::Differences;
use FAST::Struct::Seq;
#use Data::Dumper::Simple;

BEGIN { use_ok('FAST::Struct::If'); }

my $class = 'FAST::Struct::If';

my $if = $class->new('<p>', '[L:=1]', '[L:=3]');
ok $if;
isa_ok $if, $class;

# Test $if->entry/$if->exit:
is( $if->entry, $if->condition, "`entry' is `condition'" );
isnt( $if->exit,  $if->true_branch, "`exit' is not `true_branch'" );
isnt( $if->exit,  $if->false_branch, "`exit' is not `false_branch' either" );
my @elems = $if->elems;
is( $if->entry, $elems[0] );
is( $if->exit, $elems[3] );
is( $if->entry->label, '<p>' );
is( $if->exit->label, '' );
is( $if->exit, $if->tail );

# Test $if->condition:
my $cond = $if->condition;
ok $cond;
isa_ok $cond, 'FAST::Node';
is( $cond->label, '<p>' );

# Test $if->true_branch:
my $tb = $if->true_branch;
ok $tb;
isa_ok $tb, 'FAST::Node';
is( $tb->label, '[L:=1]' );

is( $if->as_c, <<'_EOC_' );
if (p) {
    do L:=1
} else {
    do L:=3
}
_EOC_

is( $if->as_c(1), <<'_EOC_' );
    if (p) {
        do L:=1
    } else {
        do L:=3
    }
_EOC_

is( $if->as_c(2), <<'_EOC_' );
        if (p) {
            do L:=1
        } else {
            do L:=3
        }
_EOC_

# Test ->subs
ok $if->subs('<p>', '<q>');
@elems = $if->elems;
is( $elems[0]->label, '<q>' );
is( $elems[1]->label, '[L:=1]' );

# Test $if->elems in scalar context:
my $relems = $if->elems;
is( $relems->[0]->label, '<q>' );
is( $relems->[1]->label, '[L:=1]' );

# Test method `might_pass'
ok $if->might_pass('<q>');
ok !$if->might_pass('<p>');
ok $if->might_pass('[L:=1]');
ok $if->might_pass('[L:=3]');
ok !$if->might_pass('[L:=2]');

# Test method `must_pass'
ok $if->must_pass('<q>');
ok !$if->must_pass('<p>');
ok !$if->must_pass('[L:=1]');
ok !$if->must_pass('[L:=3]');
ok !$if->must_pass('[L:=2]');

require 'FAST/Struct/Seq.pm';
ok $if->subs('[L:=1]', FAST::Struct::Seq->new('[L:=1]', '[L:=3]'));
is( $if->as_c, <<'_EOC_' );
if (q) {
    do L:=1
    do L:=3
} else {
    do L:=3
}
_EOC_

ok $if->must_pass('<q>');
ok !$if->must_pass('<p>');
ok !$if->must_pass('[L:=1]');
ok $if->must_pass('[L:=3]');
ok !$if->must_pass('[L:=2]');

my $outfile = 't/03basic.png';
unlink $outfile if not -f $outfile;
$if->as_png($outfile);
ok -f $outfile;

{
    # Test nested FAST::Struct::If:
    my $e1 = $class->new('<p>', '[L:=1]', '[L:=2]');
    my $e2 = $class->new('<q>', '[L:=3]', '[L:=4]');
    my $if  = $class->new('<r>', $e1, $e2);
    is( $if->condition->label, '<r>' );
    is( $if->true_branch, $e1 );
    is( $if->false_branch, $e2 );

    is( $if->true_branch->condition->label, '<p>' );
    is( $if->true_branch->true_branch->label, '[L:=1]' );
    is( $if->true_branch->false_branch->label, '[L:=2]' );

    is( $if->false_branch->condition->label, '<q>' );
    is( $if->false_branch->true_branch->label, '[L:=3]' );
    is( $if->false_branch->false_branch->label, '[L:=4]' );

    is( $if->entry, $if->condition, "`entry' is the first node" );
    is( $if->exit,  $if->tail, "`exit' is the last node" );

    eq_or_diff( $if->as_c, <<'_EOC_' );
if (r) {
    if (p) {
        do L:=1
    } else {
        do L:=2
    }
} else {
    if (q) {
        do L:=3
    } else {
        do L:=4
    }
}
_EOC_

    eq_or_diff( $if->as_c(1), <<'_EOC_' );
    if (r) {
        if (p) {
            do L:=1
        } else {
            do L:=2
        }
    } else {
        if (q) {
            do L:=3
        } else {
            do L:=4
        }
    }
_EOC_


    eq_or_diff( $if->as_c(2), <<'_EOC_' );
        if (r) {
            if (p) {
                do L:=1
            } else {
                do L:=2
            }
        } else {
            if (q) {
                do L:=3
            } else {
                do L:=4
            }
        }
_EOC_

    ok( !$if->must_pass('[L:=1]') );
    ok( !$if->must_pass('[L:=2]') );
    ok( !$if->must_pass('[L:=3]') );
    ok( !$if->must_pass('[L:=4]') );

    ok( $if->must_pass('<r>') );
    ok( !$if->must_pass('<p>') );
    ok( !$if->must_pass('<q>') );

    ok( $if->might_pass('[L:=1]') );
    ok( $if->might_pass('[L:=2]') );
    ok( $if->might_pass('[L:=3]') );
    ok( $if->might_pass('[L:=4]') );

    ok( $if->might_pass('<r>') );
    ok( $if->might_pass('<p>') );
    ok( $if->might_pass('<q>') );

    ok( !$if->might_pass('[L:=5]') );
    ok( !$if->must_pass('[L:=5]') );

    my $saved_if = $if->clone;
    ok $saved_if;
    isa_ok $saved_if, 'FAST::Struct::If';
    isnt( $saved_if->id, $if->id, 'clone works' );

    ok $if->subs('[L:=3]', FAST::Struct::Seq->new('[L:=1]', '[L:=3]'));
    eq_or_diff( $if->as_c, <<'_EOC_' );
if (r) {
    if (p) {
        do L:=1
    } else {
        do L:=2
    }
} else {
    if (q) {
        do L:=1
        do L:=3
    } else {
        do L:=4
    }
}
_EOC_

    eq_or_diff( $saved_if->as_c, <<'_EOC_' );
if (r) {
    if (p) {
        do L:=1
    } else {
        do L:=2
    }
} else {
    if (q) {
        do L:=3
    } else {
        do L:=4
    }
}
_EOC_

    ok ! $if->subs('[L:=8]', FAST::Struct::Seq->new('[L:=1]', '[L:=3]'));
    ok $if->subs('[L:=2]', FAST::Struct::Seq->new('[L:=2]', '[L:=1]'));
    ok $if->subs('[L:=4]', '[L:=1]');
    eq_or_diff( $if->as_c, <<'_EOC_' );
if (r) {
    if (p) {
        do L:=1
    } else {
        do L:=2
        do L:=1
    }
} else {
    if (q) {
        do L:=1
        do L:=3
    } else {
        do L:=1
    }
}
_EOC_

    ok( $if->must_pass('[L:=1]') );
    ok( !$if->must_pass('[L:=2]') );
    ok( !$if->must_pass('[L:=3]') );
    ok( !$if->must_pass('[L:=4]') );

    ok( $if->might_pass('[L:=1]') );
    ok( $if->might_pass('[L:=2]') );
    ok( $if->might_pass('[L:=3]') );
    ok( !$if->might_pass('[L:=4]') );

    my $outfile = 't/04basic.png';
    unlink $outfile if not -f $outfile;
    $if->as_png($outfile);
    ok -f $outfile;
}

{
    # empty false branch not dump out:
    my $if = FAST::Struct::If->new('<p>', '[L:=1]', '');
    ok $if;
    is( $if->as_c, <<'_EOC_' );
if (p) {
    do L:=1
}
_EOC_

    $if = FAST::Struct::If->new('<p>', '', '[hello]');
    ok $if;
    is( $if->as_c, <<'_EOC_' );
if (p) {
} else {
    do hello
}
_EOC_

}

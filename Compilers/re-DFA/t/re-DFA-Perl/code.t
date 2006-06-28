# code.t

use strict;
use warnings;

use Test::More tests => 6;
use Test::Differences;

BEGIN { use_ok('re::DFA::Perl'); }

my $code = re::DFA::Perl->as_code('', 'match_empty');
eq_or_diff $code, <<'_EOC_';
sub match_empty {
    my $s = shift;
    return undef if !defined $s;
    my $pos = 0;
    my $state = 1;
    my $done;
    while (1) {
        my $char = substr($s, $pos, 1);
        if ($state == 1) {
            $done = $pos;
            last;
        }
        die "error: Unknown state: $state";
    } continue {
        $pos++;
    }
    if (!defined $done) { return undef; }
    substr($s, 0, $done);
}
_EOC_

$code = re::DFA::Perl->as_code("a", 'match');
eq_or_diff $code, <<'_EOC_';
sub match {
    my $s = shift;
    return undef if !defined $s;
    my $pos = 0;
    my $state = 1;
    my $done;
    while (1) {
        my $char = substr($s, $pos, 1);
        if ($state == 1) {
            last if !defined $char;
            if ($char eq "a") {
                $state = 2;
                next;
            }
            last;
        }
        if ($state == 2) {
            $done = $pos;
            last;
        }
        die "error: Unknown state: $state";
    } continue {
        $pos++;
    }
    if (!defined $done) { return undef; }
    substr($s, 0, $done);
}
_EOC_

$code = re::DFA::Perl->as_code('a|b', 'match');
eq_or_diff $code, <<'_EOC_';
sub match {
    my $s = shift;
    return undef if !defined $s;
    my $pos = 0;
    my $state = 1;
    my $done;
    while (1) {
        my $char = substr($s, $pos, 1);
        if ($state == 1) {
            last if !defined $char;
            if ($char eq "a") {
                $state = 2;
                next;
            }
            if ($char eq "b") {
                $state = 2;
                next;
            }
            last;
        }
        if ($state == 2) {
            $done = $pos;
            last;
        }
        die "error: Unknown state: $state";
    } continue {
        $pos++;
    }
    if (!defined $done) { return undef; }
    substr($s, 0, $done);
}
_EOC_

$code = re::DFA::Perl->as_code('(a|)b*', 'match');
eq_or_diff $code, <<'_EOC_';
sub match {
    my $s = shift;
    return undef if !defined $s;
    my $pos = 0;
    my $state = 1;
    my $done;
    while (1) {
        my $char = substr($s, $pos, 1);
        if ($state == 1) {
            $done = $pos;
            last if !defined $char;
            if ($char eq "a") {
                $state = 2;
                next;
            }
            if ($char eq "b") {
                $state = 2;
                next;
            }
            last;
        }
        if ($state == 2) {
            $done = $pos;
            last if !defined $char;
            if ($char eq "b") {
                $state = 2;
                next;
            }
            last;
        }
        die "error: Unknown state: $state";
    } continue {
        $pos++;
    }
    if (!defined $done) { return undef; }
    substr($s, 0, $done);
}
_EOC_

$code = re::DFA::Perl->as_code('(a|ba)*', 'match');
eq_or_diff $code, <<'_EOC_';
sub match {
    my $s = shift;
    return undef if !defined $s;
    my $pos = 0;
    my $state = 1;
    my $done;
    while (1) {
        my $char = substr($s, $pos, 1);
        if ($state == 1) {
            $done = $pos;
            last if !defined $char;
            if ($char eq "a") {
                $state = 1;
                next;
            }
            if ($char eq "b") {
                $state = 2;
                next;
            }
            last;
        }
        if ($state == 2) {
            last if !defined $char;
            if ($char eq "a") {
                $state = 1;
                next;
            }
            last;
        }
        die "error: Unknown state: $state";
    } continue {
        $pos++;
    }
    if (!defined $done) { return undef; }
    substr($s, 0, $done);
}
_EOC_

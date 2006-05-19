# code.t

use strict;
use warnings;

use Test::More tests => 5;
use Test::Differences;

BEGIN { use_ok('re::DFA::Perl'); }

my $code = re::DFA::Perl::as_code('', 'match_empty');
eq_or_diff $code, <<'_EOC_';
sub match_empty {
    my $s = shift;
    return undef if !defined $s;
    my $pos = 0;
    my $state = 1;
    while ($state != 0) {
        my $char = substr($s, $pos, 1);
        if ($state == 1) {
            if (!defined $char) {
                last;
            }
            last;
        }
        die "error: Unknown state: $state";
    } continue {
        $pos++;
    }
    if ($state == 0) { return undef; }
    substr($s, 0, $pos);
}
_EOC_

$code = re::DFA::Perl::as_code('a', 'match');
eq_or_diff $code, <<'_EOC_';
sub match {
    my $s = shift;
    return undef if !defined $s;
    my $pos = 0;
    my $state = 1;
    while ($state != 0) {
        my $char = substr($s, $pos, 1);
        if ($state == 1) {
            if (!defined $char) {
                $state = 0;
                next;
            }
            if ($char eq 'a') {
                $state = 2;
                next;
            }
            $state = 0;
            next;
        }
        if ($state == 2) {
            if (!defined $char) {
                last;
            }
            last;
        }
        die "error: Unknown state: $state";
    } continue {
        $pos++;
    }
    if ($state == 0) { return undef; }
    substr($s, 0, $pos);
}
_EOC_

$code = re::DFA::Perl::as_code('a|b', 'match');
eq_or_diff $code, <<'_EOC_';
sub match {
    my $s = shift;
    return undef if !defined $s;
    my $pos = 0;
    my $state = 1;
    while ($state != 0) {
        my $char = substr($s, $pos, 1);
        if ($state == 1) {
            if (!defined $char) {
                $state = 0;
                next;
            }
            if ($char eq 'a') {
                $state = 2;
                next;
            }
            if ($char eq 'b') {
                $state = 2;
                next;
            }
            $state = 0;
            next;
        }
        if ($state == 2) {
            if (!defined $char) {
                last;
            }
            last;
        }
        die "error: Unknown state: $state";
    } continue {
        $pos++;
    }
    if ($state == 0) { return undef; }
    substr($s, 0, $pos);
}
_EOC_

$code = re::DFA::Perl::as_code('(a|)b*', 'match');
eq_or_diff $code, <<'_EOC_';
sub match {
    my $s = shift;
    return undef if !defined $s;
    my $pos = 0;
    my $state = 1;
    while ($state != 0) {
        my $char = substr($s, $pos, 1);
        if ($state == 1) {
            if (!defined $char) {
                last;
            }
            if ($char eq 'a') {
                $state = 2;
                next;
            }
            if ($char eq 'b') {
                $state = 2;
                next;
            }
            last;
        }
        if ($state == 2) {
            if (!defined $char) {
                last;
            }
            if ($char eq 'b') {
                $state = 2;
                next;
            }
            last;
        }
        die "error: Unknown state: $state";
    } continue {
        $pos++;
    }
    if ($state == 0) { return undef; }
    substr($s, 0, $pos);
}
_EOC_

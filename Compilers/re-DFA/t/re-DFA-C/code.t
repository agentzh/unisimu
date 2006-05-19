# code.t

use strict;
use warnings;

use Test::More tests => 6;
use Test::Differences;

BEGIN { use_ok('re::DFA::C'); }

my $code = re::DFA::C->as_code('', 'match_empty');
eq_or_diff $code, <<'_EOC_';
int match_empty (char* s) {
    int pos = -1;
    int state = 1;
    int done = -1;

    while (1) {
        char c;
        c = s[++pos];
        if (state == 1) {
            done = pos;
            break;
        }
        fprintf( stderr, "error: Unknown state: %d", state );
        exit(2);
    }
    return done;
}
_EOC_

$code = re::DFA::C->as_code('a', '');
eq_or_diff $code, <<'_EOC_';
int match (char* s) {
    int pos = -1;
    int state = 1;
    int done = -1;

    while (1) {
        char c;
        c = s[++pos];
        if (state == 1) {
            if (c == '\0') break;
            if (c == 'a') {
                state = 2;
                continue;
            }
            break;
        }
        if (state == 2) {
            done = pos;
            break;
        }
        fprintf( stderr, "error: Unknown state: %d", state );
        exit(2);
    }
    return done;
}
_EOC_

$code = re::DFA::C->as_code('a|b', 'match');
eq_or_diff $code, <<'_EOC_';
int match (char* s) {
    int pos = -1;
    int state = 1;
    int done = -1;

    while (1) {
        char c;
        c = s[++pos];
        if (state == 1) {
            if (c == '\0') break;
            if (c == 'a') {
                state = 2;
                continue;
            }
            if (c == 'b') {
                state = 2;
                continue;
            }
            break;
        }
        if (state == 2) {
            done = pos;
            break;
        }
        fprintf( stderr, "error: Unknown state: %d", state );
        exit(2);
    }
    return done;
}
_EOC_

$code = re::DFA::C->as_code('(a|)b*', 'match');
eq_or_diff $code, <<'_EOC_';
int match (char* s) {
    int pos = -1;
    int state = 1;
    int done = -1;

    while (1) {
        char c;
        c = s[++pos];
        if (state == 1) {
            done = pos;
            if (c == '\0') break;
            if (c == 'a') {
                state = 2;
                continue;
            }
            if (c == 'b') {
                state = 2;
                continue;
            }
            break;
        }
        if (state == 2) {
            done = pos;
            if (c == '\0') break;
            if (c == 'b') {
                state = 2;
                continue;
            }
            break;
        }
        fprintf( stderr, "error: Unknown state: %d", state );
        exit(2);
    }
    return done;
}
_EOC_

$code = re::DFA::C->as_code('(a|ba)*', 'match');
eq_or_diff $code, <<'_EOC_';
int match (char* s) {
    int pos = -1;
    int state = 1;
    int done = -1;

    while (1) {
        char c;
        c = s[++pos];
        if (state == 1) {
            done = pos;
            if (c == '\0') break;
            if (c == 'a') {
                state = 1;
                continue;
            }
            if (c == 'b') {
                state = 2;
                continue;
            }
            break;
        }
        if (state == 2) {
            if (c == '\0') break;
            if (c == 'a') {
                state = 1;
                continue;
            }
            break;
        }
        fprintf( stderr, "error: Unknown state: %d", state );
        exit(2);
    }
    return done;
}
_EOC_

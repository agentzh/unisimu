# opp.t

use strict;
use warnings;

use constant {
    EXPECT_TERM   => 0x01,
    EXPECT_OPER   => 0x02,
    EXPECT_START  => 0x05,

    OP_EMPTY         => 0x00,
    OP_TERM          => 0x10,
    OP_POSTFIX       => 0x20,
    OP_CLOSE         => 0x30,
    OP_PREFIX        => 0x40,
    OP_PRELIST       => 0x50,
    OP_INFIX         => 0x60,
    OP_TERNARY       => 0x70,
    OP_POSTCIRCUMFIX => 0x80,
    OP_CIRCUMFIX     => 0x90,
};

use Test::More 'no_plan';
use opp;

$X::str = '51+325-(2*5)';
$X::pos = 0;
test_token(OP_TERM, '51');
test_token(OP_INFIX, '+');
test_token(OP_TERM, '325');
test_token(OP_INFIX, '-');
test_token(OP_CIRCUMFIX, '(');
test_token(OP_TERM, '2');
test_token(OP_INFIX, '*');
test_token(OP_TERM, '5');
test_token(OP_CLOSE, ')');
is get_token, undef;

my $token = Token->new(OP_INFIX, '+');
is $token->syncat, OP_INFIX;
is $token->match, '+';
is $token->precedence, 20;
is $token->assoc, 'L';

$token = Token->new(OP_INFIX, '*');
is $token->syncat, OP_INFIX;
is $token->match, '*';
is $token->precedence, 21;
is $token->assoc, 'L';

$token = Token->new(OP_INFIX, '^');
is $token->syncat, OP_INFIX;
is $token->match, '^';
is $token->precedence, 22;
is $token->assoc, 'R';

$token = Token->new(OP_TERM, 512);
is $token->syncat, OP_TERM;
is $token->match, 512;

sub test_token {
    my ($cat, $content) = @_;
    my $token = get_token;
    is $cat, $token->syncat;
    is $content, $token->match;
}

$X::str = '3';
$X::pos = 0;
is parse(), 3;

$X::str = '3+1';
$X::pos = 0;
is parse(), 4;

$X::str = '3+2+5';
$X::pos = 0;
is parse(), 10;

$X::str = '3-2+5';
$X::pos = 0;
is parse(), 6;

$X::str = '3-6*8';
$X::pos = 0;
is parse(), -45;

$X::str = '3-6*8/4';
$X::pos = 0;
is parse(), -9;

$X::str = '3^2^2';
$X::pos = 0;
is parse(), 81;

$X::str = '3^2^3';
$X::pos = 0;
is parse(), 6561;

$X::str = '3-(6+8)';
$X::pos = 0;
is parse(), -11;

$X::str = '15/3*5';
$X::pos = 0;
is parse(), 25;

$X::str = '15/(3*5)';
$X::pos = 0;
is parse(), 1;

$X::str = '(15/(3*5))';
$X::pos = 0;
is parse(), 1;

#$::OPP_DEBUG = 1;
$X::str = '((15))';
$X::pos = 0;
is parse(), 15;

$X::str = '2.5+7.1';
$X::pos = 0;
is parse(), 9.6;

$X::str = ' 2.5 + 7.1 ';
$X::pos = 0;
is parse(), 9.6;

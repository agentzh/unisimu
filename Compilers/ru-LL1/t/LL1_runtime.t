#: LL1_runtime.t

use strict;
use warnings;

use Test::More tests => 100;
use LL1_runtime;

# Test match_token:

*match_token = \&LL1::Runtime::match_token;

my $s = 'if(571 ) { foo5 = a*b }';
$X::str = $s;
$X::pos = 0;

is match_token("'if'"), 'if';
is $X::str, $s, '$X::str intact';
is $X::pos, 2, '$pos moved';

is match_token("')'"), undef;
is $X::str, $s, '$X::str intact';
is $X::pos, 2, '$X::pos intact';

is match_token('/\d+/'), undef;
is $X::str, $s, '$X::str intact';
is $X::pos, 2, '$X::pos intact';

is match_token('/[()]/'), '(';
is $X::str, $s, '$X::str intact';
is $X::pos, 3, '$X::pos moved';

is match_token('/\d+/'), 571;
is $X::str, $s, '$X::str intact';
is $X::pos, 6, '$X::pos moved';

is match_token("')'"), ')';
is $X::str, $s, '$X::str intact';
is $X::pos, 8, '$X::pos moved';

is match_token("'a'"), undef;
is $X::str, $s, '$X::str intact';
is $X::pos, 9, '$X::pos moved (side-effect)';

is match_token('/{/'), '{';
is $X::pos, 10, '$X::pos moved';

# Test get_token:

*get_token = \&LL1::Runtime::get_token;

my $IF       = "'if'";
my $LEFT_B   = "'('";
my $RIGHT_B  = "')'";
my $CURLY_B  = '/{|}/';
my $EQ       = "'='";
my $NUM      = '/\d+/';
my $ID       = '/[A-Za-z]\w*/';

$X::tokens = [$IF, $LEFT_B, $RIGHT_B, $CURLY_B, $EQ, $NUM, $ID];
$X::pos = 0;

is get_token(), $IF;
is $X::raw, 'if';
is $X::pos, 2;

is get_token(), $LEFT_B;
is $X::raw, '(';
is $X::pos, 3;

is get_token(), $NUM;
is $X::raw, '571';
is $X::pos, 6;

is get_token(), $RIGHT_B;
is $X::raw, ')';
is $X::pos, 8;

is get_token(), $CURLY_B;
is $X::raw, '{';
is $X::pos, 10;

is get_token(), $ID;
is $X::raw, 'foo5';
is $X::pos, 15;

is get_token(), $EQ;
is $X::raw, '=';
is $X::pos, 17;

is get_token(), $ID;
is $X::raw, 'a';
is $X::pos, 19;

is get_token(), LL1::err();
is $X::raw, '*b';
is $X::pos, 21;

is get_token(), $CURLY_B;
is $X::raw, '}';
is $X::pos, 23;

is get_token(), LL1::eof();
is $X::raw, '';
is $X::pos, 23;

is get_token(), LL1::eof();
is $X::raw, '';
is $X::pos, 23;

# Test eval_table:

*eval_tb = \&LL1::Runtime::eval_table;

sub err { $LL1::Runtime::Error }

###
#    S: '(' S ')' S | ''
###

$X::tokens = [qw/ '(' ')' /];
my $table = {
    'S' => {
        "'('" => [ "'('", 'S', "')'", 'S' ],
        "')'" => [],
        LL1::eof() => [],
    },
};

$LL1::Runtime::Trace = 1;

$X::str = '()';
$X::pos = 0;
ok eval_tb($table, 'S');
is $X::pos, 2;

$X::str = '(';
$X::pos = 0;
ok !eval_tb($table, 'S');
is $X::pos, 1;
is err(), "Was expecting ')', but found EOF instead";

$X::str = '(a)';
$X::pos = 0;
ok !eval_tb($table, 'S');
is $X::pos, 1;
is err(), "Was expecting S, but found 'a)' instead";

$X::str = '()foo bar';
$X::pos = 0;
ok !eval_tb($table, 'S');
is $X::pos, 2;
is err(), "Was expecting S, but found 'foo' instead";

$X::str = '() ( )';
$X::pos = 0;
ok eval_tb($table, 'S');
is $X::pos, 6;

$X::str = '(( ) )';
$X::pos = 0;
ok eval_tb($table, 'S');
is $X::pos, 6;

$X::str = '(()())()';
$X::pos = 0;
ok eval_tb($table, 'S');
is $X::pos, 8;

##
#  statement: if_stmt | other
#  if_stmt: 'if' '(' exp ')' statement else_part
#  else_part: 'else' statement
#           | # emtpy
#  exp      : '0' | '1'
##

$X::tokens = [qw/ 'other' 'if' 'else' '0' '1' '(' ')' /];
$table = {
    'statement' => {
        "'if'"    => [ 'if_stmt' ],
        "'other'" => [ "'other'" ],
    },
    'if_stmt'   => {
        "'if'"    => [ "'if'", "'('", 'exp', "')'",
                       'statement', 'else_part' ],
    },
    'else_part' => {
        "'else'"   => [ "'else'", 'statement' ],
        LL1::eof() => [],
    },
    'exp' => {
        "'0'" => [ "'0'" ],
        "'1'" => [ "'1'" ],
    },
};

$X::str = 'if (0) other';
$X::pos = 0;
ok eval_tb($table, 'statement');
is $X::pos, 12;
#warn err();

$X::str = 'if(0) if(1) other else other';
$X::pos = 0;
ok eval_tb($table, 'statement');
is $X::pos, 28;

$X::str = 'if (1) other else other';
$X::pos = 0;
ok eval_tb($table, 'statement');
is $X::pos, 23;

$X::str = 'foo';
$X::pos = 0;
ok !eval_tb($table, 'statement');
is $X::pos, 0;
is err(), "Was expecting statement, but found 'foo' instead";

$X::str = 'if (3) other';
$X::pos = 0;
ok !eval_tb($table, 'statement');
is $X::pos, 4;
is err(), "Was expecting exp, but found '3)' instead";

$X::str = 'if (0) other other';
$X::pos = 0;
ok !eval_tb($table, 'statement');
is $X::pos, 12;
is err(), "Was expecting else_part, but found 'other' instead";

$X::str = 'if )0) other';
$X::pos = 0;
ok !eval_tb($table, 'statement');
is $X::pos, 2;
is err(), "Was expecting '(', but found ')' instead";

$X::str = 'if (1) other else other if';
$X::pos = 0;
ok !eval_tb($table, 'statement');
is $X::pos, 23;
is err(), "Was expecting EOF, but found 'if' instead";

$X::str = 'if (1)';
$X::pos = 0;
ok !eval_tb($table, 'statement');
is $X::pos, 6;
is err(), "Was expecting statement, but found EOF instead";

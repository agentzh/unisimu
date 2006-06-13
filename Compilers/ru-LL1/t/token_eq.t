# token_eq.t
# Test sub LL1::Parser::token_eq in LL1_parser.pm

use strict;
use warnings;

use Test::More tests => 19;
use LL1_parser;

*token_eq = \&LL1::Parser::token_eq;

# string != string
ok ! token_eq(qw[ 'a' 'b' ]), "'a' != 'b'";
ok ! token_eq(qw[ 'a' 'aa' ]), "'a' != 'aa'";
ok ! token_eq(qw[ 'aa' 'a' ]), "'aa' != 'a'";

# regex != regex
ok ! token_eq(qw[ 'a' 'b' ]), "/a|b/ != /b/";
ok ! token_eq(qw[ /a/ /a*/ ]), "/a/ != /a*/";
ok ! token_eq(qw[ /a*/ /a/ ]), "/a*/ != /a/";

# regex != string
ok ! token_eq(qw[ 'ab' /a/ ]), "'ab' != /a/";
ok ! token_eq(qw[ /a/ "ab" ]), '/a/ != "ab"';
ok ! token_eq(qw[ /a{2}/ "aaa" ]), '/a{2}/ != "aaa"';

# string == string
ok token_eq(qw[ 'a' 'a' ]), "'a' == 'a'";
ok token_eq(qw[ "a" 'a' ]), "\"a\" == 'a'";
ok token_eq(qw[ "a" "a" ]), '"a" == "a"';
ok token_eq(qw[ "\"\"" '""' ]), q/"\"\"" '""'/;

# regex == string
ok token_eq(qw[ 'a' /a/ ]), "'a' == /a/";
ok token_eq(qw[ /a/ "a" ]), '/a/ == "a"';
TODO: {
    local $TODO = "Hit Regexp::Compare's bug";
    ok token_eq('/a{2,2}/', '"aa"'), '/a{2}/ == "aa"';
}

# regex == regex
ok token_eq(qw[ /[0-9]+/ /\d+/ ]), "/[0-9]+/ == /\\d+/";
ok token_eq(qw[ /\w+/ /[A-Za-z0-9_]+/ ]), "/\\w+/ == /[A-Za-z0-9_]+/";
ok token_eq(qw[ /[A-Za-z]\w*/ /[a-zA-Z]\w*/ ]),
    "/[A-Za-z]\\w*/ == /[a-zA-Z]\\w*/";

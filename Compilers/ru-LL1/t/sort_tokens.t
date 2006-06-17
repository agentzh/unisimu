# sort_tokens.t
# test LL1::Parser::sort_tokens

use strict;
use warnings;
use Test::More tests => 4;
use LL1_parser;

*sort_tokens = \&LL1::Parser::sort_tokens;

my @tokens;
@tokens = sort_tokens(qw< /\w+/ 'if' 'else' /\d+/ >);
is join(' ', @tokens), "'if' 'else' /\\d+/ /\\w+/";

@tokens = sort_tokens(qw< /\w+/ 'if' '(' ')' 'else' '0' '1' >);
is join(' ', @tokens), "'if' 'else' '0' '1' /\\w+/ '(' ')'";

@tokens = sort_tokens(qw< 'if' /\w+/ '(' '0' "1" ')' 'else' >);
is join(' ', @tokens), q['if' '0' "1" 'else' /\w+/ '(' ')'];

@tokens = sort_tokens(qw< 'if' '(' '0' "1" ')' 'else' /\w+/ >);
is join(' ', @tokens), q['if' '(' '0' "1" ')' 'else' /\w+/];

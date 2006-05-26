# bootstrap.t
# Test the bootstrap capability of spike.pl

use strict;
use warnings;

use FindBin;
use lib "$FindBin::Bin/../lib";
use test_spike;

{
    package Parser1;
    use Text::Balanced qw/ extract_delimited extract_codeblock /;
}

plan tests => 1 * blocks() + 3 * 1;

run_tests;

__DATA__

=== TEST 1: regex terminal
--- grammar

grammar: rule(s) eofile

                  { 
                     my @rules = @{ $item[1] };
                     {
                         startrule => $rules[0]->[0],
                         rules => { map {@$_} @rules },
                     };
                  }
       | <error>

eofile: /^\Z/

rule: rulename ':' <commit> production(s /\|/)

                   { 
                     [ $item[1], $item[4] ];
                   }
    | <error?> <reject>

rulename: /[A-Za-z]\w*/

production: item(s)
          | nil

item: repetition
    | subrule
    | terminal
    | action
    | directive

subrule: /[A-Za-z]\w*\b(?!\s*:)/

terminal: string
        | regex

string: /"(\\.|[^"])*"/
      | /'(\\.|[^'])*'/

regex: {extract_delimited($text,'/')}    { $item[1] || undef }

action: {extract_codeblock($text)}       { $item[1] || undef }

directive: '<error?>'
         | '<error>'
         | '<reject>'
         | '<commit>'
         | '<uncommit>'
         | '<leftop:' subrule regex subrule '>'
                                { [ @item[2..4] ] }

repetition: subrule howoften    { [ $item[1], @{$item[2]} ]; }

howoften: '(?)'                         { [ '?' ]; }
        | '(s?' <commit> regex(?) ')'   { [ 's?', @{$item[3]} ]; }
        | '(s'  <commit> regex(?) ')'   { [ 's', @{$item[3]} ]; }

nil: ''  { ["''"] }

--- input

identifier: /[A-Za-z]\w*/

--- ast
{
  'rules' => {
    'identifier' => [
      [
        '/[A-Za-z]\\w*/',
      ]
    ]
  },
  'startrule' => 'identifier'
};



=== TEST 2: string terminal
--- input

keyword: 'if'
       | 'else'

--- ast
{
  'rules' => {
    'keyword' => [
      [
        '\'if\''
      ],
      [
        '\'else\''
      ]
    ]
  },
  'startrule' => 'keyword'
};



=== TEST 3: concat rule
--- input

if_stmt: 'if' '(' cond ')' block

cond: /\d+/

block: '{' /[^}]*/ "}"

--- ast
{
  'rules' => {
    'cond' => [
      [
        '/\\d+/'
      ]
    ],
    'if_stmt' => [
      [
        '\'if\'',
        '\'(\'',
        'cond',
        '\')\'',
        'block'
      ]
    ],
    'block' => [
      [
        '\'{\'',
        '/[^}]*/',
        '"}"'
      ]
    ]
  },
  'startrule' => 'if_stmt'
};

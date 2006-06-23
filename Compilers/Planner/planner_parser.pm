#: planner_parser.pm
#: simple parser for Planner inputs
#: Copyright (c) 2006 Agent Zhang
#: 2006-06-23 2006-06-23

package Planner::Parser;

use strict;
use warnings;
use Parse::RecDescent;

my $Grammar = <<'END_GRAMMAR';

program: begin_block(?) var_definition rules regex end_block(?) eofile
             {
                 {
                     begin_block => $item[1]->[0],
                     vars        => $item[2],
                     rules       => $item[3],
                     regex       => $item[4],
                     end_block   => $item[5]->[0],
                 };
             }
       | <error>

eofile: /^\Z/

begin_block: block

end_block: block

block: {extract_codeblock($text)}

var_definition: 'var' <commit> variable(s /,/) ';' { $item[3] }
              | <error?> <reject>

variable: identifier

rules: rule(s)  { my %rules = map {@$_} @{$item[1]}; \%rules; }

rule: identifier ':' <commit> condition action
        { [ $item[1] => [$item[4], $item[5]] ] }
    | <error?> <reject>

identifier: /[A-Za-z]\w*/

condition: block

action: block

regex: alternation ';'  { $item[1] }

alternation: concat(s /\|/)
                {
                    my @elems = @{ $item[1] };
                    if (@elems > 1) {
                        bless $item[1], 'altern';
                    } else {
                        $elems[0];
                    }
                }
           | <error>

concat: qualified_atom(s)
                {
                    my @elems = @{ $item[1] };
                    if (@elems > 1) {
                        bless $item[1], $item[0];
                    } else {
                        $elems[0];
                    }
                }
      | <error>

qualified_atom: atom '*'  { bless [ 0, 'inf', $item[1] ], 'repet'; }
              | atom '+'  { bless [ 1, 'inf', $item[1] ], 'repet'; }
              | atom '{' <commit> number <uncommit> '}'
                      { bless [ $item[4], $item[4], $item[1] ], 'repet'; }
              | atom '{' <commit> number ',' <uncommit> number '}'
                      { bless [ $item[4], $item[6], $item[1] ], 'repet'; }
              | atom '{' <commit> number ',' '}'
                      { bless [ $item[4], 'inf', $item[1] ], 'repet'; }
              | atom
              | <error?> <reject>

atom: identifier
    | '(' <commit> alternation ')'  { $item[3] }
    | <error?> <reject>

number: /[1-9]\d*/

END_GRAMMAR

my $Parser;

sub new {
    my $class = shift;
    $::RD_HINT = 1;
    $Parser ||= new Parse::RecDescent ($Grammar) or die "Bad grammar!\n";
    $class;
}

sub parse {
    shift;
    my $src = shift;
    #$::RD_TRACE = 1;
    $Parser->program($src);
}

1;

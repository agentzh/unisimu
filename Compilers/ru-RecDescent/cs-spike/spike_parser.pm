#: spike_parser.pm
#: simple parser for BNF
#: 2006-05-25 2006-05-28

package Spike::Parser;

use strict;
use warnings;
use Parse::RecDescent;

my $Grammar = <<'END_GRAMMAR';

grammar: preamble(?) rule(s) eofile

                  {
                     my @rules = @{ $item[2] };
                     {
                         preamble => $item[1][0],
                         startrule => $rules[0][0],
                         rules => { map {@$_} @rules },
                     };
                  }

       | <error>

preamble: action

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
    | comment

subrule: /[A-Za-z]\w*\b(?!\s*:)/

terminal: string
        | regex

string: /"(\\.|[^"])*"/
      | /'(\\.|[^'])*'/

regex: {extract_delimited($text,'/')}    { $item[1] || undef }

action: /\s*/ { $X::thisline = $thisline; extract_codeblock($text) }

    { $item[2] . " $X::thisline" || undef }

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

comment: /\/\*.*?\*\//  { '' }

nil: ''  { ["''"] }

END_GRAMMAR

my $Parser;

sub new {
    my $class = shift;
    #warn "HERE!";
    $Parser ||= new Parse::RecDescent ($Grammar) or die "Bad grammar!\n";
    $class;
}

sub parse {
    shift;
    my ($src, $infile) = @_;
    $infile ||= "GRAMMAR";
    $X::infile = $infile;
    #$::RD_TRACE = 1;
    $Parser->grammar($src);
}

1;

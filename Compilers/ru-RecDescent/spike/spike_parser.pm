#: spike_parser.pm
#: simple parser for BNF
#: 2006-05-25 2006-05-25

package Spike::Parser;

use strict;
use warnings;
use Parse::RecDescent;

my $Grammar = <<'END_GRAMMAR';

grammar: rule(s) eofile

                  { 
                     my @rules = @{ $item{'rule(s)'} };
                     {
                         startrule => $rules[0]->[0],
                         rules => { map {@$_} @rules },
                     };
                   }
       | <error>

eofile: /^\Z/

rule: rulename ':' <commit> production(s /\|/)

                   { 
                     [ $item{rulename}, $item{'production(s)'} ];
                    }
    | <error?> <reject>

rulename: /[A-Za-z]\w*/

production: item(s)
          | nil

item: repetition
    | subrule
    | terminal
    | directive

subrule: /[A-Za-z]\w*\b(?!\s*:)/

terminal: string
        | regex

string: /"([\\]"|[^"])*"/
      | /'([\\]'|[^'])*'/

regex: /\/(\\\/|[^\/])*\//

directive: '<commit>'
         | '<uncommit>'
         | '<leftop' subrule regex subrule '>'
                                { [ @item[2..4] ] }

repetition: subrule howoften    { [ $item[1], @{$item[2]} ]; }

howoften: '(?)'                 { [ '?' ]; }
        | '(s' regex(?) ')'     { [ 's', @{$item[2]} ]; }
        | '(s?' regex(?) ')'    { [ 's?', @{$item[2]} ]; }

nil: ''  { ["''"] }

END_GRAMMAR

my $Parser;

sub parse {
    shift;
    my $src = shift;
    #$::RD_TRACE = 1;
    $Parser ||= new Parse::RecDescent ($Grammar) or die "Bad grammar!\n";
    $Parser->grammar($src);
}

1;

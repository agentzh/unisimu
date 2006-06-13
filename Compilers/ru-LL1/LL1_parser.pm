#: LL1_parser.pm
#: simple parser for BNF
#: 2006-06-04 2006-06-12

package LL1::Parser;

use strict;
use warnings;

use List::Util 'first';
use Carp 'croak';

use Data::Dumper::Simple;
use Parse::RecDescent;
use Regexp::Compare qw(is_less_or_equal);

my $Grammar = <<'END_GRAMMAR';

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
                     push @$X::rules, $item[1];
                     [ $item[1], $item[4] ];
                   }

    | <error?> <reject>

rulename: /[A-Za-z]\w*/

production: item(s)
          | nil

item: subrule
    | terminal
    | action
    | directive

subrule: /[A-Za-z]\w*\b(?!\s*:)/

terminal: string
        | regex

string: /'(\\.|[^'])*'/

regex:  {extract_delimited($text,'/')}   { $item[1] || undef }

action: {extract_codeblock($text)}       { $item[1] || undef }

directive: '<error>'

nil: ''  { [] }

END_GRAMMAR

my $Parser;

sub new {
    my $class = shift;
    $Parser ||= new Parse::RecDescent ($Grammar) or die "Bad grammar!\n";
    $class;
}

sub parse {
    shift;
    my $src = shift;
    #$::RD_TRACE = 1;
    $X::rules = [];
    my $ast = $Parser->grammar($src);
    my $context = {};
    my @tokens =
        collect_tokens($ast->{rules}, $ast->{startrule}, $context);
    $X::tokens = \@tokens;
    $ast;
}

sub collect_tokens {
    my ($rules, $rulename, $context) = @_;
    #$Data::Dumper::Indent = 1;
    #warn Dumper($rules);
    return if $context->{":$rulename"};
    $context->{":$rulename"} = 1;
    $context->{tokens} ||= [];
    my $prods = $rules->{$rulename};
    if (!defined $prods) {
        croak "error: nonderminal '$rulename' not defined in the grammar.\n";
    }
    for my $prod (@$prods) {
        $context->{$prod} = 1;
        for my $item (@$prod) {
            if ($item =~ /^\W/) {
                #warn "XXX $item";
                #warn "XXX @{ $context->{tokens} }";
                my $twin = first { token_eq($item, $_) } @{ $context->{tokens} };
                if (!defined $twin) {
                    push @{ $context->{tokens} }, $item;
                } else { #if ($item ne $twin) {
                    warn "warning: Duplicate token $item ignored (see $twin).\n";
                    $item = $twin;
                }
            } else {
                collect_tokens($rules, $item, $context);
            }
        }
    }
    @{ $context->{tokens} };
}

sub token_eq {
    my ($a, $b) = @_;
    #warn "AAA Comparing $a $b...";
    if ($a =~ /^["']/) {
        $a = quotemeta(eval $a);
    } else {
        $a =~ s,^\/|\/$,,g;
    }
    if ($b =~ /^["']/) {
        $b = quotemeta(eval $b);
    } else {
        $b =~ s,^/|/$,,g;
    }
    my $res = 0;
    eval {
        $res += (is_less_or_equal($a, $b) ? 1 : 0);
    };
    eval {
        $res += (is_less_or_equal($b, $a) ? 1 : 0);
    };
    #warn "AAA $a == $b ? ", ($res == 2 ? 'true!' : 'false!'), " ($res)";
    return $res == 2;
}

1;

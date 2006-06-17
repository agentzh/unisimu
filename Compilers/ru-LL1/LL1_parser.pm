#: LL1_parser.pm
#: simple parser for BNF listing
#: 2006-06-04 2006-06-17

package LL1::Parser;

use strict;
use warnings;

use List::Util qw/ first /;
use List::MoreUtils qw/ first_index /;
use Carp 'croak';

use Data::Dumper::Simple;
use Parse::RecDescent;
use Regexp::Compare qw(is_less_or_equal);

my $Grammar = <<'END_GRAMMAR';

grammar: component(s) eofile

                  { 
                     my @rules = grep { $_ } @{ $item[1] };
                     {
                         startrule => $rules[0]->[0],
                         rules => { map {@$_} @rules },
                     };
                  }

       | <error>

component: rule
         | comment      { '' }
         | directive    { '' }
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
    | comment    { "''" }

subrule: /[A-Za-z]\w*\b(?!\s*:)/

terminal: string
        | regex

string: /'(\\.|[^'])*'/
      | /"(\\.|[^"])*"/

regex:  {extract_delimited($text,'/')}   { $item[1] || undef }

action: {extract_codeblock($text)}       { $item[1] || undef }

directive: '<error>'
         | '<token:' terminal(s) '>'  { $X::tokens = $item[2] }

comment: /#[^\n]*/

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
    undef $X::tokens;
    my $ast = $Parser->grammar($src);
    my @tokens = collect_tokens($ast->{rules}, $ast->{startrule}, {});
    #warn "!!! XXX @tokens";
    if (!$X::tokens) {
        @tokens = sort_tokens(@tokens);
        if (!$::LL1_QUIET) {
            warn "warning: Directive <token: @tokens> added automatically.\n";
        }
        $X::tokens = \@tokens;
    } else {
        validate_tokens(@$X::tokens);
    }
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
        @$prod = grep { $_ ne "''" and $_ ne '""' } @$prod;
        for my $item (@$prod) {
            if ($item =~ /^\W/) {
                #warn "XXX $item";
                #warn "XXX @{ $context->{tokens} }";
                my $twin = first { token_eq($_, $item) } @{ $context->{tokens} };
                if (!defined $twin) {
                    push @{ $context->{tokens} }, $item;
                } else { #if ($item ne $twin) {
                    if (!$::LL1_QUIET) {
                        warn "warning: Duplicate token $item ignored (see $twin).\n";
                    }
                    $item = $twin;
                }
            } else {
                collect_tokens($rules, $item, $context);
            }
        }
    }
    @{ $context->{tokens} };
}

sub sort_tokens {
    my @tokens = @_;
    my @sorted;
    for my $token (@tokens) {
        #warn "check $token\n";
        my $done;
        for my $i (0..$#sorted) {
            #warn "  $token <=> $sorted[$i]\n";
            my $res = token_cmp($sorted[$i], $token);
            if ($res > 0) {
                if ($i == 0) {
                    unshift @sorted, $token;
                } else {
                    splice(@sorted, $i, 0, $token);
                }
                $done = 1;
                last;
            }
        }
        if (!$done) {
            push @sorted, $token;
        }
        #warn "[ @sorted ]\n\n";
    }
    @sorted;
}

sub token_eq {
    my ($a, $b) = @_;
    #warn "AAA Comparing $a $b...";
    my ($re1, $re2);
    if ($a =~ /^["']/) {
        $re1 = quotemeta(eval $a);
    } else {
        ($re1 = $a) =~ s,^\/|\/$,,g;
    }
    if ($b =~ /^["']/) {
        $re2 = quotemeta(eval $b);
    } else {
        ($re2 = $b) =~ s,^/|/$,,g;
    }
    my ($le, $ge);
    eval {
        $le = is_less_or_equal($re1, $re2);
    };
    eval {
        $ge = is_less_or_equal($re2, $re1);
    };
    return 1 if $le && $ge;
    #if ($ge) {
    #    warn "warning: Token $b may never match due to token $a.\n";
    #}
    undef;
}

sub token_cmp {
    my ($a, $b) = @_;
    #warn "AAA Comparing $a $b...";
    my ($re1, $re2);
    if ($a =~ /^["']/) {
        $re1 = quotemeta(eval $a);
    } else {
        ($re1 = $a) =~ s,^\/|\/$,,g;
    }
    if ($b =~ /^["']/) {
        $re2 = quotemeta(eval $b);
    } else {
        ($re2 = $b) =~ s,^/|/$,,g;
    }
    my ($le, $ge);
    eval {
        $le = is_less_or_equal($re1, $re2);
    };
    eval {
        $ge = is_less_or_equal($re2, $re1);
    };
    return 1 if $ge;
    return -1 if $le;
    return 0
}

1;

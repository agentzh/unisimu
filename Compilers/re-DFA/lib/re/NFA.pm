#: re/NFA.pm
#: NFA emitter for re
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-15 2006-05-15

package re::NFA;

use strict;
use warnings;
#use Data::Dumper::Simple;

use re;
use re::Graph;
use Language::AttributeGrammar;
#use Scalar::Util qw( looks_like_number );

our $Counter = 0;

sub translate {
    my ($src, $imfile) = @_;
    #warn $src;
    my $parser = re::Parser->new() or die "Can't construct the parser!\n";
    my $ptree = $parser->program($src) or return undef;
    my $g = emit($ptree);
    $g->as_png($imfile);
}

sub emit {
    my $ptree = shift;
    $Data::Dumper::Indent = 1;
    #warn Dumper($ptree);
    $Counter = 0;
    $re::NFA::emit::grammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

program:    $/.NFA = { $<expression>.NFA }

expression: $/.NFA = { $<alternation>.NFA }
alternation:     $/.NFA = { re::NFA::emit_alternation( $<alternation>.NFA, $<concat>.NFA ); }
concat:     $/.NFA = { re::NFA::emit_concat( $<concat>.NFA, $<modified_atom>.NFA ); }

modified_atom:  $/.NFA = { re::NFA::emit_modified_atom( $<atom>.NFA, $<modifier>.NFA ); }
atom:           $/.NFA = { re::NFA::emit_atom( $<child>.NFA ) }
modifier:       $/.NFA = { $<__VALUE__> }

char:           $/.NFA = { $<__VALUE__> }

nil:        $/.NFA = { '' }

END_GRAMMAR
    $re::NFA::emit::grammar->apply($ptree, 'NFA');
}

sub emit_alternation {
    my ($a, $b) = @_;
    if (!$a) { return $b };
    my $c = $a->merge($b);
    my $left  = ++$Counter;
    my $right = ++$Counter;
    $c->entry($left);
    $c->exit($right);
    $c->add_edge($left, re::eps, $a->entry);
    $c->add_edge($left, re::eps, $b->entry);
    $c->add_edge($a->exit, re::eps, $right);
    $c->add_edge($b->exit, re::eps, $right);
    $c;
}

sub emit_concat {
    my ($a, $b) = @_;
    if (!$a) { return $b };
    my $c = $a->merge($b);
    my $left  = $a->entry;
    my $right = $b->exit;
    $c->entry($left);
    $c->exit($right);
    $c->add_edge($a->exit, re::eps, $b->entry);
    $c;
}

sub emit_modified_atom {
    my ($atom, $modifier) = @_;
    return $atom if ! $modifier;
    if ($modifier eq '*') {
        my $left  = ++$Counter;
        my $right = ++$Counter;
        $atom->add_edge($atom->exit, re::eps, $atom->entry);
        $atom->add_edge($left, re::eps, $atom->entry);
        $atom->add_edge($atom->exit, re::eps, $right);
        $atom->entry($left);
        $atom->exit($right);
        $atom->add_edge($left, re::eps, $right);
        $atom;
    } else {
        die "modifier $modifier not support yet";
    }
}

sub emit_atom {
    my ($re) = @_;
    if (ref $re) {
        $re;
    } else {
        my $left  = ++$Counter;
        my $right = ++$Counter;
        re::Graph->new($left, $re, $right);
    }
}

1;
__END__

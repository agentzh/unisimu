#: Kid/MathModel.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-24 2006-04-24

package Kid::MathModel;

use strict;
use warnings;
use Carp qw( croak );

#use Data::Dumper::Simple;
use Kid;
use Kid::Logic::Disjoint;
use Language::AttributeGrammar;
use Clone;

our $Grammar;
our %Context;

sub transform {
    my $disjoint_ast = shift;
    my @sets;
    for my $route (@$disjoint_ast) {
        push @sets, process_route($route);
    }
    return \@sets;
}

sub process_route {
    my $route = shift;
    %Context = ();
    my (@rels, @assigns);
    for my $expr (@$route) {
        next if ! $expr;
        my $expr = rename_vars($expr);
        if ($expr->isa('condition')) {
            push @rels, $expr;
        } elsif ($expr->isa('assignment')) {
            push @assigns, $expr;
        } else {
            croak "Unknown expr: ", ref $expr;
        }
    }
    my (@inits, @finals);
    while (my ($key, $val) = each %Context) {
        push @inits, "${key}_0";
        push @finals, "${key}_${val}";
    }
    return {
        init_vars   => \@inits,
        final_vars  => \@finals,
        relationals => \@rels,
        assignments => \@assigns,
    };
}

sub rename_vars {
    my $tree = shift;
    $Grammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

number:     $/.mm = { number->new( $<__VALUE__> ); }
factor:     $/.mm = { factor->new( $<child>.mm ); }

neg:        $/.mm = { neg->new('-') }
term:       $/.mm = { term->new( $<neg>.mm, $<term>.mm, $<op>, $<factor>.mm ); }
expression: $/.mm = { expression->new( $<expression>.mm, $<op>, $<term>.mm ); }

nil:        $/.mm = { nil->new; }
identifier: $/.mm = { Kid::MathModel::emit_id( $<__VALUE__> ); }
var:        $/.mm = { var->new( $<identifier>.mm ) }

assignment: $/.mm = { Kid::MathModel::emit_assign( $<expression>.mm, $<var>.mm ); }

rhs_expression:  $/.mm = { rhs_expression->new( $<expression>.mm ) }
rel_op:       $/.mm = { rel_op->new( $<__VALUE__> ); }
condition:    $/.mm = { condition->new( $<expression>.mm, $<rel_op>.mm, $<rhs_expression>.mm ); }

END_GRAMMAR
    return '' if ! $tree;
    return $Grammar->apply($tree, 'mm');
}

sub emit_id {
    my $id_val = shift;
    $Context{$id_val} ||= 0;
    #warn "emit_id: ${id_val}_$Context{$id_val}";
    identifier->new( "${id_val}_$Context{$id_val}" );
}

sub emit_assign {
    my ($expr_ast, $var_ast) = @_;
    my $id_ast = $var_ast->identifier;
    my $id_val = $id_ast->value;
    $id_val =~ s/_\d+$//;
    $Context{$id_val}++;
    #warn "emit_assign: ${id_val}_$Context{$id_val}";
    $id_ast->value( "${id_val}_$Context{$id_val}" );
    return assignment->new($var_ast, $expr_ast);
}

sub translate {
    my $src = $_[0];
    #warn $src;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $parse_tree = $parser->program( $src ) or return undef;
    my $logic_ast = Kid::Logic::transform( $parse_tree );
    my $disjoint_ast = Kid::Logic::Disjoint::transform( $logic_ast );
    my $mm_ast = transform( $disjoint_ast );
    my $s = '';
    for my $set (@$mm_ast) {
        my $inits   = join ', ', sort @{ $set->{init_vars}   };
        my $finals  = join ', ', sort @{ $set->{final_vars}  };
        my $rels    = join ', ', to_maple( @{ $set->{relationals}} );
        my $assigns = join ', ', to_maple( @{ $set->{assignments}} );
        $s .= <<_EOC_;
--
 - $inits
 - $finals
 - $rels
 - $assigns
_EOC_

    }
    $s =~ s/\s+\n/\n/sg;
    $s;
}

sub to_maple {
    map { 
        my $code = Kid::Maple::emit_maple($_);
        $code =~ s/;\n$//;
        $code;
    } @_;
}

1;
__END__

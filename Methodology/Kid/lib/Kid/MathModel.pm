#: Kid/MathModel.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-24 2006-04-24

package Kid::MathModel;

use strict;
use warnings;
use Carp qw( croak );

#use Data::Dumper::Simple;
use Kid;
use Kid::Proc;
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
    $route = Clone::clone($route);
    %Context = ();
    my (@rels, @assigns);
    for my $expr (@$route) {
        next if ! $expr;

        #warn $expr->dump;
        delete $expr->{parent};
        $expr = rename_vars( Clone::clone($expr) );
        #$expr = Kid::Proc::transform($expr);
        #warn $expr->dump;
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
identifier: $/.mm = { identifier->new( $<__VALUE__> ); }
var:        $/.mm = { Kid::MathModel::emit_var( $<identifier>.mm ) }

assignment: $/.mm = { Kid::MathModel::emit_assign( $<expression>.mm, $<var>.mm ); }

rhs_expression:  $/.mm = { rhs_expression->new( $<expression>.mm ) }
rel_op:       $/.mm = { rel_op->new( $<__VALUE__> ); }
condition:    $/.mm = { condition->new( $<expression>.mm, $<rel_op>.mm, $<rhs_expression>.mm ); }

END_GRAMMAR
    #return '' if ! $tree;
    return $Grammar->apply($tree, 'mm');
}

sub emit_var {
    my $id = shift;
    my $varname = $id->value;
    $Context{$varname} ||= 0;
    #warn "emit_id: ${id_val}_$Context{$id_val}";
    $id->value("${varname}_$Context{$varname}");
    var->new( $id );
}

sub emit_assign {
    my ($expr_ast, $var_ast) = @_;
    my $varname = $var_ast->identifier->value;
    $varname =~ s/_\d+$//;
    $Context{$varname}++;
    #warn "emit_assign: ${id_val}_$Context{$id_val}";
    $var_ast->identifier->value( "${varname}_$Context{$varname}" );
    assignment->new($var_ast, $expr_ast);
}

sub translate {
    my $src = $_[0];
    #warn $src;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $ptree = $parser->program( $src ) or return undef;
    my $ast = Kid::Proc::transform( $ptree );
    my $logic_ast = Kid::Logic::transform( $ast );
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
        #warn "entering emit_maple...";
        #warn $_->dump;
        #warn $_->kid;
        my $code = Kid::Maple::emit_maple($_);
        #warn "leaving maple...";
        $code =~ s/;\n$//;
        $code;
    } @_;
}

1;
__END__

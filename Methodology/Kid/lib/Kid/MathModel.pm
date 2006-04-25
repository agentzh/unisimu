#: Kid/MathModel.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-24 2006-04-24

package Kid::MathModel;

use strict;
use warnings;

use Data::Dumper::Simple;
use Kid;
use Kid::Logic::Disjoint;
use Language::AttributeGrammar;
use Clone;

our $Grammar;

sub translate {
    my $src = $_[0];
    #warn $src;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $parse_tree = $parser->program( $src ) or return undef;
    my $logic_ast = Kid::Logic::transform( $parse_tree );
    my $disjoint_ast = Kid::Logic::Disjoint::transform( $logic_ast );
    my @sets;
    for my $route (@$disjoint_ast) {
        push @sets, process_route($route);
    }
    join(",\n", @sets) . ";\n";
}

sub process_route {
    my $route = shift;
    my %c;
    my @exprs;
    for my $expr (@$route) {
        if ($expr->isa('assignment')) {
            my $assign = Clone::clone( $expr );
            #warn "assignment: ", Kid::Maple::emit_maple($expr);

            my $rhs = $assign->{expression};
            my $rhs_vars = get_vars($rhs);
            update_vars($rhs_vars, \%c);

            my $lhs_name = $assign->{var}->{identifier}->{__VALUE__};
            #warn "lhs: $lhs_name";
            $c{$lhs_name}++;

            #$Data::Dumper::Indent = 1;
            #warn Dumper($rhs);
            push @exprs, "${lhs_name}_$c{$lhs_name}=" .
                Kid::Maple::emit_maple( Clone::clone($rhs) );
        } elsif ($expr->isa('condition')) {
            my $cond = Clone::clone( $expr );
            #warn "condition: ", Kid::Maple::emit_maple($expr);
            my $vars = get_vars( $cond );
            update_vars($vars, \%c);
            push @exprs, Kid::Maple::emit_maple( Clone::clone( $cond ) );
            #$Data::Dumper::Indent = 0;
            #warn Dumper(@exprs);
        } else {
            die "Unknown expr: ", ref $expr;
        }
    }
    '{ ' . join(', ', @exprs) . ' }';
}

sub update_vars {
    my ($vars, $rc) = @_;
    while (my ($key, $value) = each %$vars) {
        for my $id_obj (@$value) {
            die ref $id_obj if ref $id_obj ne 'identifier';
            my $num = $rc->{$key} || 0;
            #warn "update_vars: ${key}_${num} !!!";
            #$Data::Dumper::Indent = 1;
            #warn Dumper($id_obj);
            $id_obj->{__VALUE__} = "${key}_${num}";
        }
    }
}

sub get_vars {
    my $ast = shift;
    $Grammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

number:     $/.vars = { {} }
factor:     $/.vars = { $<child>.vars }

term:       $/.vars = { Kid::MathModel::merge( $<term>.vars, $<factor>.vars ); }
expression: $/.vars = { Kid::MathModel::merge( $<expression>.vars, $<term>.vars ); }

nil:        $/.vars = { {} }
identifier: $/.vars = { {} }
var:        $/.vars = { Kid::MathModel::emit_var( $<identifier> ) }

assignment: $/.vars = { Kid::MathModel::merge( $<var>.vars, $<expression>.vars ); }

block:           $/.vars = { $<statement_list>.vars }
else_block:      $/.vars = { $<block>.vars }
rhs_expression:  $/.vars = { $<expression>.vars }

rel_op:       $/.vars = { {} }
condition:    $/.vars = { Kid::MathModel::merge( $<expression>.vars, $<rhs_expression>.vars ); }
if_statement: $/.vars = { Kid::MathModel::merge( $<condition>.vars, $<block>.vars, $<else_block>.vars ); }

statement:      $/.vars = { $<child>.vars }
statement_list: $/.vars = { Kid::MathModel::merge( $<statement_list>.vars, $<statement>.vars ); }

program:    $/.vars = { $<statement_list>.vars }

END_GRAMMAR
    return $Grammar->apply($ast, 'vars');
}

sub emit_var {
    my $id = shift;
    return { $id->{__VALUE__} => [ $id ] };
}

sub merge {
    my %res;
    for (@_) {
        next if ! $_ or ! ref $_;
        while (my ($key, $value) = each %$_) {
            if ($res{$key}) {
                push @{ $res{$key} }, @$value;
            } else {
                $res{$key} = $value;
            }
        }
    }
    return \%res;
}

1;
__END__

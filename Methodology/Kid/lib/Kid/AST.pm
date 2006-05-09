#: Kid/AST.pm
#: Abastract Syntax Tree (also parse tree) for Kid
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-25 2006-04-25

package Kid::AST;

use strict;
use warnings;

use Kid::AST::Element;
use Kid::AST::Statements;
use Kid::AST::Expressions;
use Kid::AST::Identifiers;
use Kid::AST::Expression;
use Kid::AST::Term;

my @rules = qw(
    identifier number var
    factor term expression rhs_expression
    condition rel_op
    neg nil
    proc_call proc_decl declaration
    expression_list identifier_list
    if_statement assignment list_assignment
    block else_statement
    statement statement_list program
);

for my $rule (@rules) {
    no strict 'refs';
    push @{"${rule}::ISA"}, "Kid::AST::Element";
}

package if_statement;

sub else_statement {
    my $self = shift;
    my $else = $self->{else_statement};
    return $else if $else;
    $else =
        $self->{'_alternation_1_of_production_1_of_rule_if_statement(?)'}
        [0]{'else_statement'};
    if ($else) {
        return $else;
    } else {
        return nil->new;
    }
}

package term;

sub neg {
    my $self = shift;
    if ($self->{neg}) { return $self->{neg}; }
    else { return nil->new; }
}

1;
__END__

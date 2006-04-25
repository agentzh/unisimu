#: Kid/AST.pm
#: Abastract Syntax Tree (also parse tree) for Kid
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-25 2006-04-25

package Kid::AST;

use strict;
use warnings;

use Kid::AST::Element;
use Kid::AST::Statements;
use Kid::AST::Expression;
use Kid::AST::Term;

my @rules = qw(
    identifier number var
    factor term expression rhs_expression
    condition rel_op
    nil
    if_statement assignment
    block else_block
    statement statements
);

for my $rule (@rules) {
    no strict 'refs';
    push @{"${rule}::ISA"}, "Kid::AST::Element";
}

package if_statement;

sub else_block {
    my $self = shift;
    my $else_block =
        $self->{'_alternation_1_of_production_1_of_rule_if_statement(?)'}
        [0]{'else_block'};
    if ($else_block) {
        return $else_block;
    } else {
        return bless {}, 'nil';
    }
}

1;
__END__

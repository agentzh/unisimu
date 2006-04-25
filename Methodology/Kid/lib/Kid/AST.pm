#: Kid/AST.pm
#: Abastract Syntax Tree (also parse tree) for Kid
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-25 2006-04-25

package Kid::AST;

use Kid::AST::Element;
use Kid::AST::Statements;
use Kid::AST::Expression;
use Kid::AST::Term;

package factor;
use base 'Kid::AST::Element';

package statement;
use base 'Kid::AST::Element';

package if_statement;
use base 'Kid::AST::Element';

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

package rel_op;
use base 'Kid::AST::Element';

1;
__END__

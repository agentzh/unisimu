package Kid;

use strict;
use warnings;

use Kid::Parser;
use Kid::Element;
use Kid::Term;
use Kid::Expression;
use Kid::Statements;
#use Data::Dumper::Simple;

package factor;
use base 'Kid::Element';

package statement;
use base 'Kid::Element';

package if_statement;
use base 'Kid::Element';

sub else_block {
    my $self = shift;
    #$Data::Dumper::Indent = 1;
    #my $s = Dumper($self);
    #$s =~ s/^\s*__RULE__:[^\n]+\n//gsm;
    #$s =~ s/^\s*__DIRECTIVE\d+__:[^\n]*\n//gsm;
    #$s =~ s/ !perl\/[^\n]+//sg;
    #warn $s;
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
use base 'Kid::Element';

1;

#: re/AST.pm
#: Abastract Syntax Tree (also parse tree) for re-DFA
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-13 2006-05-13

package re::AST;

use strict;
use warnings;

use re::AST::Element;
use re::AST::Branch;
use re::AST::Concat;

my @rules = qw(
    expression brach eof
    concat modified_atom atom atom_star
    char nil
);

for my $rule (@rules) {
    no strict 'refs';
    push @{"${rule}::ISA"}, "re::AST::Element";
}

package modified_atom;

use strict;
use warnings;

sub modifier {
    my $self = shift;
    my $modifier = $self->{'modifier(?)'};
    if (@$modifier) { $modifier->[0]; }
    else { nil->new; }
}

1;
__END__

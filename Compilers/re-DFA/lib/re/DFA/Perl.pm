#: re/DFA/Perl.pm
#: Perl code emitter for re::DFA
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-19 2006-05-19

package re::DFA::Perl;

use strict;
use warnings;

use Template;
use re::DFA::Min;

our $Template = <<'_EOC_';
sub [% subname %] {
    my $s = shift;
    return undef if !defined $s;
    my $pos = 0;
    my $state = [% dfa.entry %];
    my $done;
    while (1) {
        my $char = substr($s, $pos, 1);
      [%- FOREACH node = dfa.nodes %]
        if ($state == [% node %]) {
          [%- IF dfa.is_exit(node) %]
            $done = $pos;
          [%- END %]
          [%- edges = dfa.node2edges(node); %]
          [%- IF edges AND edges.size > 0 %]
            last if !defined $char;
          [%- END %]
          [%- IF edges.size == 2 AND edges.0.size < 2 %]
            if ($char eq '[% edges.0 %]') {
                $state = [% edges.1 %];
                next;
            }
          [%- ELSE %]
            [%- FOR edge = edges %]
            if ($char eq '[% edge.0 %]') {
                $state = [% edge.1 %];
                next;
            }
            [%- END %]
          [%- END %]
            last;
        }
      [%- END %]
        die "error: Unknown state: $state";
    } continue {
        $pos++;
    }
    if (!defined $done) { return undef; }
    substr($s, 0, $done);
}
_EOC_

sub as_code {
    my ($self, $input, $subname) = @_;
    #warn "input: $input";
    #warn "subname: $subname";
    my $dfa;
    if (!ref $input) {
        $dfa = re::DFA::Min->transform($input)->normalize;
    } else {
        $dfa = $input;
    }
    #$dfa->as_png('yay.png');
    my $code;
    my $tt = Template->new;
    $tt->process(
        \$Template,
        { dfa => $dfa, subname => $subname },
        \$code,
    ) || die $tt->error();
    $code;
}

sub as_method {
    my ($self, $input) = @_;
    my $code = $self->as_code($input);
    my $coderef = eval($code);
    if ($@) { die $@; }
    $coderef;
}

1;
__END__

#: re/DFA/C.pm
#: C code emitter for re::DFA
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-19 2006-05-19

package re::DFA::C;

use strict;
use warnings;

use Inline;
use Template;
use re::DFA::Min;

our $Template = <<'_EOC_';
[% DEFAULT subname = 'match' -%]
int [% subname %](char* s) {
    int pos = -1;
    int state = [% dfa.entry %];
    int done = -1;

    while (1) {
        char c;
        c = s[++pos];
      [%- FOREACH node = dfa.nodes %]
        if (state == [% node %]) {
          [%- IF dfa.is_exit(node) %]
            done = pos;
          [%- END %]
          [%- edges = dfa.node2edges(node); %]
          [%- IF edges AND edges.size > 0 %]
            if (c == '\0') break;
          [%- END %]
          [%- IF edges.size == 2 AND edges.0.size < 2 %]
            if (c == '[% edges.0 %]') {
                state = [% edges.1 %];
                continue;
            }
          [%- ELSE %]
            [%- FOR edge = edges %]
            if (c == '[% edge.0 %]') {
                state = [% edge.1 %];
                continue;
            }
            [%- END %]
          [%- END %]
            break;
        }
      [%- END %]
        fprintf( stderr, "error: Unknown state: %d", state );
        exit(2);
    }
    return done;
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
    my $c_code = $self->as_code($input, 're_DFA_C_as_method_match');
    Inline->bind(C => $c_code);
    sub {
        my $s = shift;
        return undef if !defined $s;
        my $pos = re_DFA_C_as_method_match($s);
        if ($pos == -1) {
            undef;
        } else {
            substr($s, 0, $pos);
        }
    }
}

1;
__END__

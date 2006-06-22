# opp.pm
# operator precedence parser library
# 2006-06-21 2006-06-21

use strict;
use warnings;
use Carp 'carp';

use constant {
    EXPECT_TERM   => 0x01,
    EXPECT_OPER   => 0x02,
    EXPECT_START  => 0x05,

    OP_EMPTY         => 0x00,
    OP_TERM          => 0x10,
    OP_POSTFIX       => 0x20,
    OP_CLOSE         => 0x30,
    OP_PREFIX        => 0x40,
    OP_PRELIST       => 0x50,
    OP_INFIX         => 0x60,
    OP_TERNARY       => 0x70,
    OP_POSTCIRCUMFIX => 0x80,
    OP_CIRCUMFIX     => 0x90,
};

my %sctable = (
    'term:'    => { 'syncat'=>OP_TERM, 'expect'=>0x0201 },
    'postfix:' => { 'syncat'=>OP_POSTFIX, 'expect'=>0x0202, 'arity'=>1 },
    'close:'   => { 'syncat'=>OP_CLOSE, 'expect'=>0x0202 },
    'prefix:'  => { 'syncat'=>OP_PREFIX, 'expect'=>0x0101, 'arity'=>1 },
    'infix:'   => { 'syncat'=>OP_INFIX, 'expect'=>0x0102, 'arity'=>2 },
    'ternary:' => { 'syncat'=>OP_TERNARY, 'expect'=>0x0102,
                    'expectclose'=>0x0102, 'arity'=>3 },
    'postcircumfix:' => { 'syncat'=>OP_POSTCIRCUMFIX, 'expect'=>0x0102, 'arity'=>2 },
    'circumfix:' => { 'syncat'=>OP_CIRCUMFIX, 'expect'=>0x0101, 'arity'=>1 },
);

my %tokentable;

my %OPTable = (
    ''  => [0, 'L'],
    '+' => [20, 'L'],
    '-' => [20, 'L'],
    '*' => [21, 'L'],
    '/' => [21, 'L'],
    '^' => [22, 'R'],
    '(' => [30, ''],
    ')' => [30, ''],
);

sub newtok {
    Token->new(@_);
}

sub get_token {
    my $expect = 
    my $s = $X::str;
    my $pos = $X::pos;
    pos($s) = $pos;
    $s =~ m/\G\s+/gc;
    my ($syncat, $content);
    if ($s =~ /\G\d+(?:\.\d+)?\b/gc) {
        $syncat = OP_TERM;
        $content = $&;
    } elsif ($s =~ /\G[-+^*\/]/gc) {
        $syncat = OP_INFIX;
        $content = $&;
    } elsif ($s =~ /\G\(/gc) {
        $syncat = OP_CIRCUMFIX;
        $content = $&;
    } elsif ($s =~ /\G\)/gc) {
        $syncat = OP_CLOSE;
        $content = $&;
    } elsif ($s =~ /\G./) {
        carp "Lexical error: [", get_next(), "]\n";
    }
    $X::pos = pos($s);
    if (defined $syncat) {
        my $token = Token->new($syncat, $content);
        return $token;
    }
    undef;
}

sub parse {
    my (@termStack, @operatorStack);
    push @operatorStack, Token->new(OP_EMPTY, '');
    trace("------");
    while (1) {
        my $token = get_token();
        my $top = $operatorStack[-1];
        if ($token) {
            trace("read token $token");
            if ($token->syncat == OP_TERM) {
                trace("shift term $token", 1);
                push @termStack, $token->match;
            } elsif ($token->syncat == OP_INFIX) {
                my $curr_prec = $token->precedence;
                my $last_prec = $top->precedence;

                my $last_syncat = $top->syncat;
                if ($last_syncat == OP_CIRCUMFIX) {
                    trace("shift $token", 2);
                    push @operatorStack, $token;
                } elsif ($last_syncat == OP_CLOSE) {
                    if ($curr_prec > $last_prec) {
                        trace("shift $token", 2);
                        push @operatorStack, $token;
                    } elsif ($curr_prec < $last_prec) {
                        reduce(\@termStack, \@operatorStack);
                        trace("shift $token", 2);
                        push @operatorStack, $token;
                    }
                } else {
                    if ($curr_prec > $last_prec) {
                        trace("shift $token", 2);
                        push @operatorStack, $token;
                    } elsif ($curr_prec < $last_prec) {
                        reduce(\@termStack, \@operatorStack);
                        trace("shift $token", 2);
                        push @operatorStack, $token;
                    } elsif ($top->assoc eq 'left') {
                        reduce(\@termStack, \@operatorStack);
                        trace("shift $token", 2);
                        push @operatorStack, $token;
                    } else {
                        #warn $top->assoc;
                        trace("shift $token", 2);
                        push @operatorStack, $token;
                    }
                }
            } elsif ($token->syncat == OP_CIRCUMFIX) {
                # XXX P5
                trace("shift $token", 2);
                push @operatorStack, $token;
            } elsif ($token->syncat == OP_CLOSE) {
                reduce(\@termStack, \@operatorStack);
                trace("shift $token", 2);
                push @operatorStack, $token;
            }
        } else {
            #warn "HERE! @operatorStack";
            while (@operatorStack > 1) {
                #warn ">>> @operatorStack";
                reduce(\@termStack, \@operatorStack);
            }
            return $termStack[-1];
        }
    }
}

sub reduce {
    my ($termStack, $operatorStack) = @_;
    #warn "!!! @$operatorStack";
    #warn "!!! @$termStack";
    my $op = $operatorStack->[-1];
    trace("reduce $op", 4);
    if ($op->syncat == OP_CLOSE) {
        pop @$operatorStack;        
        while (@$operatorStack > 1 && $operatorStack->[-1]->syncat != OP_CIRCUMFIX) {
            reduce($termStack, $operatorStack);
        }
        pop @$operatorStack;
    } elsif ($op->syncat == OP_INFIX) {
        pop @$operatorStack;
        my $term2 = pop @$termStack;
        my $term1 = pop @$termStack;
        my $result;
        if ($op eq '+') {
            $result = $term1 + $term2;
        } elsif ($op eq '-') {
            $result = $term1 - $term2;
        } elsif ($op eq '*') {
            $result = $term1 * $term2;
        } elsif ($op eq '/') {
            $result = $term1 / $term2;
        } elsif ($op eq '^') {
            $result = $term1 ** $term2;
        } else {
            carp "invalid operator: $op";
        }
        trace("push $result to TermStack");
        push @$termStack, $result;
    }
}

sub trace {
    return if !$::OPP_DEBUG;
    my ($s, $indent) = @_;
    $indent ||= 0;
    $indent = ' ' x $indent;
    my $next = get_next();
    warn $indent, "$s [$next]\n";
}

sub get_next {
    my $next = substr($X::str, $X::pos, 15);
    if (length($X::str)-$X::pos <= 15) {
        $next;
    } else {
        "$next...";
    }
}

newtok('infix:+', precedence => 20);
newtok('infix:-', equiv => 'infix:+');
newtok('infix:*', precedence => 21);
newtok('infix:/', equiv => 'infix:*');
newtok('infix:^', precedence => 22, assoc => 'right');
newtok('circumfix:( )', precedence => 30);

package Token;

use strict;
use warnings;
use overload 
    '""' => sub { $_[0]->match },
    fallback => 1;
use Carp 'carp';
use Clone 'clone';
use base qw(Class::Accessor);

__PACKAGE__->mk_ro_accessors(
    qw<syncat match precedence
       assoc arity expect key keyclose>
);

sub new {
    my $proto = shift;
    my $class = ref $proto || $proto;
    my $name = shift;
    my %args = @_;
    my ($syncat, $key);
    if ($name =~ /^(\w+:)(.*)$/) {
        ($syncat, $key) = ($1, $2);
    } else {
        carp "error: invalid token name: $name";
        return undef;
    }

    my $token = clone $sctable{syncat};
    $token->{name} = $name;
    while (my ($key, $val) = each %args) {
        $token->{$key} = $val;
    }

    $token->{assoc} ||= 'left';  # default association

    # we don't replace existing tokens
    if (!exists $tokentable{$name}) {
        $tokentable{$name} = $token;
    }

    my $keyclose;
    if ($key =~ /^\S+ (\S+)$/) {
        ($key, $keyclose) = ($1, $2);
        $token->{keyclose} = $keyclose;
    }
    $token->{key} = $key;

    my $self = bless $token, $class;
    $self;
}

1;

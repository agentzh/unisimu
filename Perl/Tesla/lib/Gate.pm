#: Gate.pm
#: various logic gates used in Tesla
#: v0.05
#: Agent2002. All rights reserved.
#: 04-11-08 04-11-13

use strict;
use warnings;

# /////////////////////////
# class Gate
# /////////////////////////
package Gate;

use Carp;
use EventConsole;

our $DEBUG = 0;

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;

    my $delay = shift;
    $delay = 0 unless defined $delay;

    my $self = {
        _inputs => [],
        _output => undef,
        _delay  => $delay,
    };
    return bless $self, $class;
}

sub inputs {
    my $self = shift;
    if (@_) {
        @{$self->{_inputs}} = @_;
        return;
    }
    return @{$self->{_inputs}};
}

sub output {
    my $self = shift;
    if (@_) {
        $self->{_output} = shift;
        return;
    }
    return $self->{_output};
}

sub delay {
    my $self = shift;
    if (@_) {
        $self->{_delay} = shift;
        return;
    }
    return $self->{_delay};
}

sub activate {
    my $self = shift;
    warn "Activating ", ref($self), "...\n" if $DEBUG;

    my ($rinputs, $output) = ($self->{_inputs}, $self->{_output});
    my @input_vals = map { $_->value } @$rinputs;
    my $new_val = $self->func(@input_vals);
    if ($new_val ne 'U') {
        local $" = ',';
        warn "Evaluating ", ref($self), "(@input_vals)...\n" if $DEBUG;
        my $delay = $self->{_delay};
        my $time = Clock->reading + $delay;
        EventConsole->add_event( $time, $output, $new_val );
        return 1;
    }
    return 0;
}

# /////////////////////////
# class AND
# /////////////////////////
package AND;

our @ISA = qw(Gate);
our $delay = 0; # in nano-seconds

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $delay;
    if (@_) {
        $delay = shift;
    }
    else { 
        no strict 'refs';
        my $pack = __PACKAGE__;
        $delay = ${"${pack}::delay"};
        # warn $delay;
    }
    my $self = $proto->SUPER::new($delay);
    return bless $self, $class;
}


sub func {
    shift;
    my @vals = grep { $_ ne 'Z' } @_;
    return 'Z' unless @vals;
    my ($has_U, $has_X);
    foreach (@vals) {
        if ($_ eq '0') { return '0'; }
        if (!$has_U and $_ eq 'U') { $has_U = 1; }
        if (!$has_X and $_ eq 'X') { $has_X = 1; }
    }
    return 'U' if $has_U;
    return 'X' if $has_X;
    return '1';
}

# /////////////////////////
# class OR
# /////////////////////////
package OR;

our @ISA = qw(Gate);
our $delay = 0; # in nano-seconds

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $delay;
    if (@_) { $delay = shift; }
    else { 
        no strict 'refs';
        my $pack = __PACKAGE__;
        $delay = ${"${pack}::delay"};
    }
    my $self = $proto->SUPER::new($delay);
    return bless $self, $class;
}

sub func {
    shift;
    my @vals = grep { $_ ne 'Z' } @_;
    return 'Z' unless @vals;
    my ($has_U, $has_X);
    foreach (@vals) {
        if ($_ eq '1') { return '1'; }
        if (!$has_U and $_ eq 'U') { $has_U = 1; }
        if (!$has_X and $_ eq 'X') { $has_X = 1; }
    }
    return 'U' if $has_U;
    return 'X' if $has_X;
    return '0';
}

# /////////////////////////
# class NOT
# /////////////////////////
package NOT;

use Carp;

our @ISA = qw(Gate);
our $delay = 0; # in nano-seconds

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $delay;
    if (@_) { $delay = shift }
    else { 
        no strict 'refs';
        my $pack = __PACKAGE__;
        $delay = ${"${pack}::delay"};
    }
    my $self = $proto->SUPER::new($delay);
    return bless $self, $class;
}

sub inputs {
    my $self = shift;
    croak "NOT Gate accept only one input signal!\n" if (@_ > 1);
    $self->SUPER::inputs(@_);
}

sub func {
    shift;
    if ($_[0] eq '0') { return '1'; }
    if ($_[0] eq '1') { return '0'; }
    return $_[0];
}

# /////////////////////////
# class NAND
# /////////////////////////
package NAND;

our @ISA = qw(Gate);
our $delay = 0; # in nano-seconds

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $delay;
    if (@_) { $delay = shift }
    else { 
        no strict 'refs';
        my $pack = __PACKAGE__;
        $delay = ${"${pack}::delay"};
    }
    my $self = $proto->SUPER::new($delay);
    return bless $self, $class;
}

sub func {
    shift;
    return NOT->func( AND->func(@_) );
}

# /////////////////////////
# class NOR
# /////////////////////////
package NOR;

our @ISA = qw(Gate);
our $delay = 0; # in nano-seconds

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $delay;
    if (@_) { $delay = shift }
    else { 
        no strict 'refs';
        my $pack = __PACKAGE__;
        $delay = ${"${pack}::delay"};
    }
    my $self = $proto->SUPER::new($delay);
    return bless $self, $class;
}

sub func {
    shift;
    return NOT->func( OR->func(@_) );
}

# /////////////////////////
# class XOR
# /////////////////////////
package XOR;

our @ISA = qw(Gate);
our $delay = 0; # in nano-seconds

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $delay;
    if (@_) {
        $delay = shift;
    }
    else { 
        no strict 'refs';
        my $pack = __PACKAGE__;
        $delay = ${"${pack}::delay"};
    }
    my $self = $proto->SUPER::new($delay);
    return bless $self, $class;
}

sub func {
    shift;
    if (@_ < 2) { return 'X' }
    my $x = shift;
    my $y = shift;
    my $res = Xor($x,$y);
    while (@_) {
        $res = Xor( $res, shift(@_) );
    }
    return $res;
}

sub Xor {
    my ($x, $y) = @_;
    return OR->func(
                AND->func( $x, NOT->func($y) ),
                AND->func( NOT->func($x), $y )
            );
}
 
1;

#: Kid/AST/Element.pm
#: Common base class for other Kid/AST/*.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-26 2006-04-26

package Kid::AST::Element;

use strict;
use warnings;
use Carp qw( croak );
use vars qw( $AUTOLOAD );

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    for (@_) {
        my $key = ref $_;
        if (! $key) {
            $self->{__VALUE__} = $_;
        } else {
            $self->{$key} = $_;
        }
    }
    return $self;
}

sub child {
    my $self = shift;
    while (my ($key, $val) = each %$self) {
        next if $key =~ /^__[A-Z]+/;
        #warn "$key - $val";
        return $val;
    }
}

sub AUTOLOAD {
	croak "Could not find method: $AUTOLOAD\n" unless ref $_[0];
    my $self = shift;
	my $class = ref $self;
	(my $property = $AUTOLOAD) =~ s/${class}:://;
    return if $property eq 'DESTROY';
    $property = '__VALUE__' if $property eq 'value';
    if (@_) {
        if (exists $self->{$property}) {
            $self->{$property} = shift;
        } else {
            croak "Could not set the property $property to $class\n";
        }
    } else {
        if (exists $self->{$property}) {
            return $self->{$property};
        } else {
            croak "Could not fetch the property $property from $class\n";
        }
    }
}

1;

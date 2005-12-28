package PV;

use strict;
use warnings;
use Carp qw(croak);
use base 'Exporter';

use Thread::Semaphore;
use Time::HiRes 'sleep';

our @EXPORT = qw(
    semas P V
);

our $sleep = 0;

our %semas;

sub semas {
	my %names = @_;
	foreach (keys %names) {
        #warn "$_ => $names{$_}\n";
        $semas{$_} = Thread::Semaphore->new($names{$_});
	}
}

sub P {
    #sleep(1);
    my $name = shift;
    my $sema = $semas{$name} or
        croak "Can't find semaphore $name";
    $sema->down;
    threads->yield;
    sleep(rand $sleep) if $sleep;
}

sub V {
    #sleep(1);
    my $name = shift;
    my $sema = $semas{$name} or
        croak "Can't find semaphore $name";
    $sema->up;
    threads->yield;
    sleep(rand $sleep) if $sleep;
}

1;

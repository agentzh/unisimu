package Clips::Batch;

use strict;
use warnings;

use File::Spec;
use File::Temp qw/ :mktemp  /;
use File::Slurp;
use vars qw( $AUTOLOAD );

our $Verbose = 0;

sub new {
    my $class = ref $_[0] ? ref shift : shift;
    my @opts = map { "-l $_" } @_;
    bless {
        plans => [],
        callbacks => [],
        opts => "@opts",
    }, $class;
}

sub _add_callback {
    my $self = shift;
    push @{ $self->{callbacks} }, @_;
}

sub _callbacks {
    my $self = shift;
    @{ $self->{callbacks} };
}

sub _add_plan {
    my $self = shift;
    push @{ $self->{plans} }, @_;
}

sub _plans {
    @{ $_[0]->{plans} };
}

sub eof {
    my $self = shift;
    my @plan = $self->_plans;
    my @callbacks = $self->_callbacks;
    my $opts = $self->{opts};
    my $tempfile = mktemp("clips_cache_XXXXXXX");
    my $cmd = "clips $opts > $tempfile";
    #warn "$cmd";
    open my $out, "| $cmd" or
        die "can't spawn clips: $!";
    for my $plan (@plan) {
        #warn "$plan";
        print $out $plan;
    }
    print $out "(exit)\n";
    close $out;

    my $output = read_file($tempfile);
    unlink $tempfile if -f $tempfile;
    my @out = split /CLIPS> /, $output;
    my $header = shift @out;
    warn $header if $Verbose or $header =~ /error/i;
    for (0..$#callbacks) {
        if ($Verbose) {
            warn "CLIPS> $plan[$_]";
            warn $out[$_] if $out[$_];
        }
        my $callback = $callbacks[$_];
        $callback->($out[$_]) if $callback;
    }
    $self->{plans}     = [];
    $self->{callbacks} = [];
}

sub AUTOLOAD {
    my $self = shift;
    my $method = $AUTOLOAD;
    $method =~ s/.*:://;
    $method =~ s/_/-/g;
    return if $method eq 'DESTROY';

    my $callback_added;
    if (@_) {
        my $last = $_[-1];
        if (ref $last eq 'SCALAR') {
            pop;
            $self->_add_callback( sub { $$last = shift } );
            $callback_added = 1;
        } elsif (ref $last eq 'ARRAY') {
            pop;
            $self->_add_callback( sub { @$last = split /\n/, shift } );
            $callback_added = 1;
        } elsif (ref $last eq 'CODE') {
            pop;
            $self->_add_callback($last);
            $callback_added = 1;
        }
    }

    if (!$callback_added) {
        $self->_add_callback(undef);
    }

    if (@_) {
        $self->_add_plan("($method @_)\n");
    } else {
        $self->_add_plan("($method)\n");
    }
}

1;

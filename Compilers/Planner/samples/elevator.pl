
package main;

our $PLAN_VERBOSE;

use strict;
use warnings;
use Getopt::Std;

my %opts;
getopts('hv:', \%opts);

if ($opts{h}) {
    print "Usage: $0 [-h] [-v <level>]\n";
    exit(0);
}

$PLAN_VERBOSE = $opts{v} || 0;


###

package X;

our ($env, $noop);

$noop = sub {
    my $c = $_[0];
    return 1 if $c eq $noop;
    @_ = $noop;
    goto &$c;
};

###

package Planner;

use strict;
use warnings;



{
    # Auto wrapper:
    my @history;
    my $res = process(\@history, @ARGV);
    if ($res) {
        if (@history) {
            print "@history\n";
        } else {
            warn "warning: no action performed.\n"
        }
    } else {
        warn "error: no solution found.\n";
    }
}

our ($on, $pos);

package X;

use strict;
use warnings;
use Clone 'clone';
use Data::Compare;

sub eq_env {
    my ($env1, $env2) = @_;
    Compare($env1, $env2, { ignore_hash_keys => ['__HISTORY__'] });
}

sub altern_0 {
    my $c = $_[0];
    my $res;
    my $old_env;

    if ($::PLAN_VERBOSE >= 3) {
        warn "  trying altern_0...\n";
    }

    $old_env = clone $X::env;
    $res = atom_up($c);
    if (defined $res) { return $res; }
    $X::env = $old_env;

    @_ = $c;
    goto &atom_down;
}

sub concat_0 {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 3) {
        warn "  trying concat_0...\n";
    }

    @_ = sub {
        @_ = $c;
        goto &atom_off;
    };
    goto &altern_0;
}

sub concat_1 {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 3) {
        warn "  trying concat_1...\n";
    }

    @_ = sub {
        @_ = $c;
        goto &repet_0;
    };
    goto &atom_init;
}

sub repet_0 {
    my $c = $_[0];
    my $old_env;
    my $i = 0;

    if ($::PLAN_VERBOSE >= 2) {
        warn "  trying repet_0...\n";
    }

    my ($fmin, $fagain, $frest);
    $fmin = sub {
        if ($i >= 0) {
            goto &$fagain;
        }
        @_ = $fmin; $i++; goto &concat_0;
    };
    $fagain = sub {
        if ($old_env && eq_env($old_env, $X::env)) {
            @_ = $X::noop;
            goto &$c;
        }
        $old_env = clone $X::env;
        goto &$frest;
    };
    $frest = sub {
        $i++;
        my $old_env = clone $X::env;
        my $res = concat_0($fagain);
        return $res if defined $res;
        $X::env = $old_env;
        @_ = $X::noop;
        goto &$c;
    };
    goto &$fmin;
}

sub atom_init {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_init...\n";
    }

    {
        package Planner;
        my $on = $X::env->{'on'};
        my $pos = $X::env->{'pos'};

        # condition:
        $X::cond = (1);
        if (!$X::cond) { return undef; }

        # action:
        $on = [5, 3]; $pos = 4;

        $X::env->{'on'} = $on;
        $X::env->{'pos'} = $pos;
        push @{ $X::env->{__HISTORY__} }, 'init';
        if ($PLAN_VERBOSE >= 1) {
            warn "@{ $X::env->{__HISTORY__} }\n";
        }
    }
    @_ = $X::noop;
    goto &$c;
}

sub atom_up {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_up...\n";
    }

    {
        package Planner;
        my $on = $X::env->{'on'};
        my $pos = $X::env->{'pos'};

        # condition:
        $X::cond = (defined ($on->[0]) && $on->[0] > $pos);
        if (!$X::cond) { return undef; }

        # action:
        warn "up $on->[0]"; $pos = $on->[0];

        $X::env->{'on'} = $on;
        $X::env->{'pos'} = $pos;
        push @{ $X::env->{__HISTORY__} }, 'up';
        if ($PLAN_VERBOSE >= 1) {
            warn "@{ $X::env->{__HISTORY__} }\n";
        }
    }
    @_ = $X::noop;
    goto &$c;
}

sub atom_down {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_down...\n";
    }

    {
        package Planner;
        my $on = $X::env->{'on'};
        my $pos = $X::env->{'pos'};

        # condition:
        $X::cond = (defined ($on->[0]) && $on->[0] < $pos);
        if (!$X::cond) { return undef; }

        # action:
        warn "down $on->[0]"; $pos = $on->[0];

        $X::env->{'on'} = $on;
        $X::env->{'pos'} = $pos;
        push @{ $X::env->{__HISTORY__} }, 'down';
        if ($PLAN_VERBOSE >= 1) {
            warn "@{ $X::env->{__HISTORY__} }\n";
        }
    }
    @_ = $X::noop;
    goto &$c;
}

sub atom_off {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_off...\n";
    }

    {
        package Planner;
        my $on = $X::env->{'on'};
        my $pos = $X::env->{'pos'};

        # condition:
        $X::cond = (defined ($on->[0]) && $on->[0] == $pos);
        if (!$X::cond) { return undef; }

        # action:
        shift @$on;

        $X::env->{'on'} = $on;
        $X::env->{'pos'} = $pos;
        push @{ $X::env->{__HISTORY__} }, 'off';
        if ($PLAN_VERBOSE >= 1) {
            warn "@{ $X::env->{__HISTORY__} }\n";
        }
    }
    @_ = $X::noop;
    goto &$c;
}


package Planner;

use strict;
use warnings;

sub process {
    my ($hist) = @_;
    if (ref $hist and ref $hist eq 'ARRAY') {
        shift;
    } else {
        $hist = [];
    }
    $X::env = {};
    $X::env->{'on'} = shift;
    $X::env->{'pos'} = shift;
    $X::env->{__HISTORY__} = [];
    my $res = X::concat_1($X::noop);
    if (defined $res) {
        if ($::PLAN_VERBOSE >= 1) {
            warn "\n<<MATCH>>\n";
        }
        @$hist = @{ $X::env->{__HISTORY__} };
        return 1;
    } else {
        if ($::PLAN_VERBOSE >= 1) {
            warn "\n<<FAIL>>\n";
        }
        return undef;
    }
}


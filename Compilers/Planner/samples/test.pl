
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

our ($str, $pos);

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
    $res = atom_a($c);
    if (defined $res) { return $res; }
    $X::env = $old_env;

    @_ = $c;
    goto &atom_b;
}

sub altern_1 {
    my $c = $_[0];
    my $res;
    my $old_env;

    if ($::PLAN_VERBOSE >= 3) {
        warn "  trying altern_1...\n";
    }

    $old_env = clone $X::env;
    $res = concat_0($c);
    if (defined $res) { return $res; }
    $X::env = $old_env;

    @_ = $c;
    goto &concat_1;
}

sub concat_0 {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 3) {
        warn "  trying concat_0...\n";
    }

    @_ = sub {
        @_ = $c;
        goto &atom_a;
    };
    goto &atom_a;
}

sub concat_1 {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 3) {
        warn "  trying concat_1...\n";
    }

    @_ = sub {
        @_ = $c;
        goto &atom_b;
    };
    goto &atom_b;
}

sub concat_2 {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 3) {
        warn "  trying concat_2...\n";
    }

    @_ = sub {
        @_ = sub {
            @_ = $c;
            goto &repet_0;
        };
        goto &altern_1;
    };
    goto &repet_0;
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
        @_ = $fmin; $i++; goto &altern_0;
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
        my $res = altern_0($fagain);
        return $res if defined $res;
        $X::env = $old_env;
        @_ = $X::noop;
        goto &$c;
    };
    goto &$fmin;
}

sub atom_a {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_a...\n";
    }

    {
        package Planner;
        my $str = $X::env->{'str'};
        my $pos = $X::env->{'pos'};

        # condition:
        $X::cond = (substr($str, $pos, 1) eq 'a');
        if (!$X::cond) { return undef; }

        # action:
        $pos++;

        $X::env->{'str'} = $str;
        $X::env->{'pos'} = $pos;
        push @{ $X::env->{__HISTORY__} }, 'a';
        if ($PLAN_VERBOSE >= 1) {
            warn "@{ $X::env->{__HISTORY__} }\n";
        }
    }
    @_ = $X::noop;
    goto &$c;
}

sub atom_b {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_b...\n";
    }

    {
        package Planner;
        my $str = $X::env->{'str'};
        my $pos = $X::env->{'pos'};

        # condition:
        $X::cond = (substr($str, $pos, 1) eq 'b');
        if (!$X::cond) { return undef; }

        # action:
        $pos++;

        $X::env->{'str'} = $str;
        $X::env->{'pos'} = $pos;
        push @{ $X::env->{__HISTORY__} }, 'b';
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
    $X::env->{'str'} = shift;
    $X::env->{'pos'} = shift;
    $X::env->{__HISTORY__} = [];
    my $res = X::concat_2($X::noop);
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


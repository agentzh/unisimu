
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

# Begin block:
sub can_put {
        my ($chessboard, $x, $y) = @_;
        my @queens = @$chessboard;
        for my $j (0..$#queens) {
            my $i = $queens[$j];
            if ($i == $x or $i + $j == $x + $y or $i - $j == $x - $y) {
                return undef;
            }
        }
        1;
    }

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

our ($x, $y, $chessboard);

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
    $res = atom_put_ln0($c);
    if (defined $res) { return $res; }
    $X::env = $old_env;

    $old_env = clone $X::env;
    $res = atom_put_ln1($c);
    if (defined $res) { return $res; }
    $X::env = $old_env;

    $old_env = clone $X::env;
    $res = atom_put_ln2($c);
    if (defined $res) { return $res; }
    $X::env = $old_env;

    @_ = $c;
    goto &atom_put_ln3;
}

sub concat_0 {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 3) {
        warn "  trying concat_0...\n";
    }

    @_ = sub {
        @_ = $c;
        goto &atom_next_col;
    };
    goto &altern_0;
}

sub concat_1 {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 3) {
        warn "  trying concat_1...\n";
    }

    @_ = sub {
        @_ = sub {
            @_ = $c;
            goto &atom_found;
        };
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
        my $x = $X::env->{'x'};
        my $y = $X::env->{'y'};
        my $chessboard = $X::env->{'chessboard'};

        # condition:
        $X::cond = (1);
        if (!$X::cond) { return undef; }

        # action:
        $chessboard = []; $y = 0;

        $X::env->{'x'} = $x;
        $X::env->{'y'} = $y;
        $X::env->{'chessboard'} = $chessboard;
        push @{ $X::env->{__HISTORY__} }, 'init';
        if ($PLAN_VERBOSE >= 1) {
            warn "@{ $X::env->{__HISTORY__} }\n";
        }
    }
    @_ = $X::noop;
    goto &$c;
}

sub atom_put_ln0 {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_put_ln0...\n";
    }

    {
        package Planner;
        my $x = $X::env->{'x'};
        my $y = $X::env->{'y'};
        my $chessboard = $X::env->{'chessboard'};

        # condition:
        $X::cond = (can_put($chessboard, 0, $y));
        if (!$X::cond) { return undef; }

        # action:
        $x = 0;

        $X::env->{'x'} = $x;
        $X::env->{'y'} = $y;
        $X::env->{'chessboard'} = $chessboard;
        push @{ $X::env->{__HISTORY__} }, 'put_ln0';
        if ($PLAN_VERBOSE >= 1) {
            warn "@{ $X::env->{__HISTORY__} }\n";
        }
    }
    @_ = $X::noop;
    goto &$c;
}

sub atom_put_ln1 {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_put_ln1...\n";
    }

    {
        package Planner;
        my $x = $X::env->{'x'};
        my $y = $X::env->{'y'};
        my $chessboard = $X::env->{'chessboard'};

        # condition:
        $X::cond = (can_put($chessboard, 1, $y));
        if (!$X::cond) { return undef; }

        # action:
        $x = 1;

        $X::env->{'x'} = $x;
        $X::env->{'y'} = $y;
        $X::env->{'chessboard'} = $chessboard;
        push @{ $X::env->{__HISTORY__} }, 'put_ln1';
        if ($PLAN_VERBOSE >= 1) {
            warn "@{ $X::env->{__HISTORY__} }\n";
        }
    }
    @_ = $X::noop;
    goto &$c;
}

sub atom_put_ln2 {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_put_ln2...\n";
    }

    {
        package Planner;
        my $x = $X::env->{'x'};
        my $y = $X::env->{'y'};
        my $chessboard = $X::env->{'chessboard'};

        # condition:
        $X::cond = (can_put($chessboard, 2, $y));
        if (!$X::cond) { return undef; }

        # action:
        $x = 2;

        $X::env->{'x'} = $x;
        $X::env->{'y'} = $y;
        $X::env->{'chessboard'} = $chessboard;
        push @{ $X::env->{__HISTORY__} }, 'put_ln2';
        if ($PLAN_VERBOSE >= 1) {
            warn "@{ $X::env->{__HISTORY__} }\n";
        }
    }
    @_ = $X::noop;
    goto &$c;
}

sub atom_put_ln3 {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_put_ln3...\n";
    }

    {
        package Planner;
        my $x = $X::env->{'x'};
        my $y = $X::env->{'y'};
        my $chessboard = $X::env->{'chessboard'};

        # condition:
        $X::cond = (can_put($chessboard, 3, $y));
        if (!$X::cond) { return undef; }

        # action:
        $x = 3;

        $X::env->{'x'} = $x;
        $X::env->{'y'} = $y;
        $X::env->{'chessboard'} = $chessboard;
        push @{ $X::env->{__HISTORY__} }, 'put_ln3';
        if ($PLAN_VERBOSE >= 1) {
            warn "@{ $X::env->{__HISTORY__} }\n";
        }
    }
    @_ = $X::noop;
    goto &$c;
}

sub atom_next_col {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_next_col...\n";
    }

    {
        package Planner;
        my $x = $X::env->{'x'};
        my $y = $X::env->{'y'};
        my $chessboard = $X::env->{'chessboard'};

        # condition:
        $X::cond = (defined $x);
        if (!$X::cond) { return undef; }

        # action:
        push @$chessboard, $x; undef $x; $y++;

        $X::env->{'x'} = $x;
        $X::env->{'y'} = $y;
        $X::env->{'chessboard'} = $chessboard;
        push @{ $X::env->{__HISTORY__} }, 'next_col';
        if ($PLAN_VERBOSE >= 1) {
            warn "@{ $X::env->{__HISTORY__} }\n";
        }
    }
    @_ = $X::noop;
    goto &$c;
}

sub atom_found {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_found...\n";
    }

    {
        package Planner;
        my $x = $X::env->{'x'};
        my $y = $X::env->{'y'};
        my $chessboard = $X::env->{'chessboard'};

        # condition:
        $X::cond = (@$chessboard == 4);
        if (!$X::cond) { return undef; }

        # action:
        warn "@$chessboard\n";

        $X::env->{'x'} = $x;
        $X::env->{'y'} = $y;
        $X::env->{'chessboard'} = $chessboard;
        push @{ $X::env->{__HISTORY__} }, 'found';
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
    $X::env->{'x'} = shift;
    $X::env->{'y'} = shift;
    $X::env->{'chessboard'} = shift;
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


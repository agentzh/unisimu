#: planner_emitter.pm
#: Emitter for Planner
#: Copyright (c) 2006 Agent Zhang
#: 2006-06-23 2006-06-23

package Planner::Emitter;

use strict;
use warnings;
use Template;
use Data::Dumper::Simple;

sub adjust_ast {
    my ($self, $ast) = @_;
    if ($ast->{begin_block}) {
        $ast->{begin_block} =~ s/^{\s*|\s*}$//sg;
    }
    if ($ast->{end_block}) {
        $ast->{end_block} =~ s/^{\s*|\s*}$//sg;
    }
    $ast->{alterns} = [];
    $ast->{concats} = [];
    $ast->{atoms}   = [];
    $ast->{repets}  = [];
    $ast->{cache} = {};
    $ast->{regex} = process_regex($ast->{regex}, $ast);
    $ast = process_rules($ast);
    #warn Dumper($ast);
    delete $ast->{cache};
    $ast;
}

sub process_regex {
    my ($regex, $ast) = @_;
    my $cache = $ast->{cache};
    if (!ref $regex) {
        if (!$cache->{$regex}) {
            push @{ $ast->{atoms} }, $regex;
            $cache->{$regex} = 1;
        }
        return "atom_$regex";
    }
    my $type = ref $regex;
    #warn "process $type";
    my @elems;
    if ($type eq 'repet') {
        $regex->[2] = process_regex($regex->[2], $ast);
        @elems = @$regex;
    } else {
        @elems = map {
            $_ = process_regex($_, $ast);
        } @$regex;
    }
    my $id;
    #warn "type = $type\n";
    if ($type eq 'repet') {
        $id = '(' . join(' ', @elems) . ')';
    } elsif ($type eq 'concat') {
        $id = '(' . join('.', @elems) . ')';
    } elsif ($type eq 'altern') {
        $id = '(' . join('|', @elems) . ')';
    } else {
        die "unknown regex type: $type";
    }
    #warn "ID = $id\n";
    my $name = $cache->{$id};
    if (!$name) {
        my $key = $type . 's';
        #warn "key = $key\n";
        my $store = $ast->{$key};
        push @$store, \@elems;
        my $i = @$store - 1;
        $name = $type . '_' . $i;
        $cache->{$id} = $name;
    }
    #warn "name = $name\n";
    return $name;
}

sub process_rules {
    my $ast = $_[0];
    my $rules = $ast->{rules};
    for my $v (values %$rules) {
        for my $code (@$v) {
            $code =~ s/^{\s*|\s*}$//gs;
        }
    }
    $ast;
}

sub emit {
    my ($self, $ast, $filetype, $package) = @_;
    $filetype ||= 'pl';
    $package  ||= 'Planner';
    $ast = $self->adjust_ast($ast);
    #warn $s;
    my $tt_ast = {
        ast       => $ast,
        package   => $package,
        filetype  => $filetype,
    };
    my $tt = Template->new;
    my $buffer;
    $tt->process(\*DATA, $tt_ast, \$buffer)
        or die $tt->error(), "\n";
    $buffer;
}

1;
__DATA__

package main;

our $PLAN_VERBOSE;

[%- IF filetype == 'pl' %]
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

[% END -%]

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

package [% package %];

use strict;
use warnings;

[% IF ast.begin_block -%]
# Begin block:
[% ast.begin_block %]
[%- END %]

[% IF ast.wrapper -%]
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
[% END -%]

[%- IF ast.vars %]
our ($[% ast.vars.join(', $'); %]);
[%- END %]

package X;

use strict;
use warnings;
use Clone 'clone';
use Data::Compare;

sub eq_env {
    my ($env1, $env2) = @_;
    Compare($env1, $env2, { ignore_hash_keys => ['__HISTORY__'] });
}

[% i = 0 -%]
[% FOREACH altern = ast.alterns -%]
sub altern_[% i %] {
    my $c = $_[0];
    my $res;
    my $old_env;

    if ($::PLAN_VERBOSE >= 3) {
        warn "  trying altern_[% i %]...\n";
    }

  [%- j = 1 %]
  [%- FOREACH elem = altern %]
    [%- IF j < altern.size %]
    $old_env = clone $X::env;
    $res = [% elem %]($c);
    if (defined $res) { return $res; }
    $X::env = $old_env;
    [%- ELSE %]
    @_ = $c;
    goto &[% elem %];
    [%- END %]
    [%- j = j + 1 %]

  [%- END -%]
}

  [%- i = i + 1 %]
[% END -%]

[%- i = 0 -%]
[% FOREACH concat = ast.concats -%]
sub concat_[% i %] {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 3) {
        warn "  trying concat_[% i %]...\n";
    }


  [%- j = 1 %]
  [%- WHILE j < concat.size %]
    [%- t = '    '; t.repeat(j) %]@_ = sub {

      [%- j = j + 1 %]
  [%- END %]
    [%- t.repeat(j) %]@_ = $c;

    [%- t.repeat(j) %]goto &[% concat.last %];

  [%- j = concat.max - 1 %]
  [%- WHILE j >= 0 %]
    [%- t.repeat(j) %]    };

    [%- t.repeat(j) %]    goto &[% concat.$j %];

    [%- j = j - 1 %]
  [%- END -%]
}

  [%- i = i + 1 %]
[% END -%]

[%- i = 0 -%]
[% FOREACH repet = ast.repets -%]
sub repet_[% i %] {
  [%- min = repet.0 %]
  [%- max = repet.1 %]
  [%- item = repet.2 %]
    my $c = $_[0];
    my $old_env;
    my $i = 0;

    if ($::PLAN_VERBOSE >= 2) {
        warn "  trying repet_[% i %]...\n";
    }

    my ($fmin, $fagain, $frest);
    $fmin = sub {
        if ($i >= [% min %]) {
            goto &$fagain;
        }
        @_ = $fmin; $i++; goto &[% item %];
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
      [%- IF max != 'inf' %]
        if ($i >= [% max %]) {
            @_ = $X::noop;
            goto &$c;
        }
      [%- END %]
        $i++;
        my $old_env = clone $X::env;
        my $res = [% item %]($fagain);
        return $res if defined $res;
        $X::env = $old_env;
        @_ = $X::noop;
        goto &$c;
    };
    goto &$fmin;
}

  [%- i = i + 1 %]
[% END -%]

[%- FOREACH atom = ast.atoms -%]
  [%- rule = ast.rules.$atom %]
  [%- cond   = rule.0 %]
  [%- action = rule.1 -%]
sub atom_[% atom %] {
    my $c = $_[0];

    if ($::PLAN_VERBOSE >= 4) {
        warn "  trying atom_[% atom %]...\n";
    }

    {
        package [% package %];
      [%- FOREACH var = ast.vars %]
        my $[% var %] = $X::env->{'[% var %]'};
      [%- END %]

        # condition:
        $X::cond = ([% cond %]);
        if (!$X::cond) { return undef; }

        # action:
        [% action.replace(';$', '') %];

      [%- FOREACH var = ast.vars %]
        $X::env->{'[% var %]'} = $[% var %];
      [%- END %]
        push @{ $X::env->{__HISTORY__} }, '[% atom %]';
        if ($PLAN_VERBOSE >= 1) {
            warn "@{ $X::env->{__HISTORY__} }\n";
        }
    }
    @_ = $X::noop;
    goto &$c;
}

[% END -%]

package [% package %];

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
  [%- FOREACH var IN ast.vars %]
    $X::env->{'[% var %]'} = shift;
  [%- END %]
    $X::env->{__HISTORY__} = [];
    my $res = X::[% ast.regex %]($X::noop);
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

[%- IF ast.end_block %]
{
  [%- FOREACH var = ast.vars %]
    my $[% var %] = $X::env->{'[% var %]'};
  [%- END %]
    [% ast.end_block %]
}
[%- END %]

[%- IF filetype == 'pm' -%]
1;
[% END -%]

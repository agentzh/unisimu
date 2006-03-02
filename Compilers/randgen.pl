#: randgen.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-02-17 2006-02-27

use strict;
use warnings;
use YAML qw/ Dump Load /;
use Parse::RandGen::Regexp;
use constant MAX => 200;

my ($fname, $goal, $num) = @ARGV;

if (not $fname) {
    die "No grammar file specified.\n";
}
if (not $goal) {
    die "No goal specified.\n";
}
$num = 1 if not defined $num or $num < 0;

open my $in, $fname or
    die "Can't open $fname for reading: $!";

my %cnt;
my $in_rule;
my $rule_name;
my %rules;

while (<$in>) {
    next if (/^\/\//);
    if (!$in_rule and /^(\w+):$/) {
        $rule_name = $1;
        #warn "Parsing rule $rule_name...\n";
        die "line $.: $rule_name redefined" if $rules{$rule_name};
        $rules{$rule_name} = [];
        $in_rule = 1;
    } elsif ($in_rule and /^\s+(.+)/) {
        my $rule = $1;
        if ($rule =~ /^(\w+):$/) {
            warn "Warning: $rule: maybe indented rule header?\n";
        }
        my @chunks = split (/\s+/, $rule);
        push @{ $rules{$rule_name} }, \@chunks;
    } elsif (/^\s*$/) {
        $in_rule = 0;
    } else {
        die "Syntax error: line $.: $_";
    }
}

close $in;

#print Dump(%rules);

if (not $rules{$goal}) {
    die "Goal $goal not found.";
}

for (1..$num) {
    %cnt = ();
    my $res = gen_nonterminal($goal);
    die if ! defined $res;
    print "$res\n";
}

sub gen_nonterminal {
    my $name = shift;
    #warn "Gen terminal: $name";
    my $def = $rules{$name};
    die "rules[$name] = $def" if not $def or not ref $def;
    my @branches = @$def;
    for (1..MAX) {
        my $i = int rand scalar @branches;
        my $branch = $branches[$i];
        die "No branch" if not ref $branch;
        my %saved_cnt = %cnt;
        my $res = gen_concat ($branch);
        return $res if defined $res;
        %cnt = %saved_cnt;
    }
    return undef;
}

sub gen_concat {
    my $branch = shift;
    my @elems = @$branch;
    #warn "Gen concat: ", join(' ', @elems);
    my $str = '';
    for my $elem (@elems) {
        $cnt{$elem}++;
        if ($cnt{$elem} > MAX) {
            return undef;
        }
        if (is_literal($elem)) {
            $str .= ' ' . gen_literal($elem);
        } else {
            my $res = gen_nonterminal($elem);
            warn "    Can't generate nontermainal $elem" if not defined $res;
            return undef if not defined $res;
            $str .= ' ' . $res;
        }
    }
    $str =~ s/\s\s+/ /g;
    #$str =~ s/([^\w\s,=*\]]) (\w)/$1$2/g;
    #$str =~ s/(\w) ([^\[\w\s=])/$1$2/g;
    #$str =~ s/([^\w\s,=]) ([^\w\s=])/$1$2/g;
    return $str;
}

sub gen_literal {
    my $elem = shift;
    $elem =~ s/^'(.+)'$/$1/;
    $elem =~ s/^"(.+)"$/$1/;
    if ($elem =~ m[^qr/(.+)/[a-z]*$]) {
        my $regex = eval($elem);
        die "$elem: $@" if $@;
        return Parse::RandGen::Regexp->new($regex)->pick();
    }
    return $elem;
}

sub is_literal {
    my $elem = shift;
    if ($elem =~ /^[A-Z][A-Za-z]+[a-z2]$/ and !$rules{$elem}) {
        #warn "Warning: $elem may be an undefined nonterminal.\n";
    }
    return not $rules{$elem};
}

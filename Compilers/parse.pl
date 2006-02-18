#: parse.pl
#: Copyright (c) 2006 Agent Zhang
#: 2006-02-17 2006-02-17

use strict;
use warnings;

use YAML qw/ Dump Load /;
use Parse::RandGen::Regexp;
use constant MAX => 200;

my $noop;
$noop = sub{ my $c = $_[0]; return 1 if $c eq $noop; @_ = $noop; goto &$c; };
sub noop {
    return $noop;
}

my $fname = shift;

if (not $fname) {
    die "No grammar file specified.\n";
}

open my $in, $fname or
    die "Can't open $fname for reading: $!";

my $in_rule;
my $rule_name;
my %rules;

while (<$in>) {
    next if (/^\/\//);
    if (!$in_rule and /^([A-Z][A-Za-z]+[a-z2]):$/) {
        $rule_name = $1;
        #warn "Parsing rule $rule_name...\n";
        die "line $.: $rule_name redefined" if $rules{$rule_name};
        $rules{$rule_name} = [];
        $in_rule = 1;
    } elsif ($in_rule and /^\s+(.+)/) {
        my $rule = $1;
        if ($rule =~ /^([A-Z][A-Za-z]+[a-z2]):$/) {
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

#while (1) {
    #print "Goal?";
    #my $goal = <STDIN>;
    #chomp $goal;
    #print "Source?";
    #my $source = <STDIN>;
    #chomp $source;
    my $goal = 'ModuleDeclaration';
    my $source = 'module abc;';
    my $parser = compile($goal);
    my $ok = parse($parser, $source);
    if (defined $ok) {
        print "Matched! :=)\n";
    } else {
        print "Unmatched! :=(\n";
    }
#}

sub emit_nonterminal {
    my $name = shift;
    warn "emit_nonterminal: $name";
    my $def = $rules{$name};
    die "rules[$name] = $def" if not $def or not ref $def;
    my @branches = @$def;
    return noop() if @branches == 0;
    #return emit_concat($branches[0]) if @branches == 1;
    my @fs = map { emit_concat($_) } @branches;
    my $f_last = pop @fs;
    return sub {
        my $c = $_[0];
        warn "    emit_nonterminal: $name - $c";
        for my $f (@fs) {
            my $c_down = $c;
            my ($str, $pos, $v);
            {
                local ($X::str, $X::pos) = ($X::str, $X::pos);
                $v = $f->($c_down);
                ($str, $pos) = ($X::str, $X::pos) if defined $v;
            }
            if (defined $v) {
                ($X::str, $X::pos) = ($str, $pos);
                return $v;
            }
        }
        @_ = $c;
        goto &$f_last;
    }
}

sub emit_concat {
    my $branch = shift;
    my @elems = @$branch;
    warn "emit_concat: ", join(' ', @elems);
    return emit($elems[0]) if @elems == 1;
    my @fs = map { emit($_) } @elems;
    my $code1 = ""; my $code2 = "";
    my $code0 = "my \$f0 = \$fs[0]; ";
    for my $i (reverse(1..$#elems)) {
        $code0 .= "my \$f$i = \$fs[$i]; ";
        $code1 .= "sub { \@_ = ";
        $code2 .= "; goto \&\$f$i}";
    }
    my $code = $code0."\nsub { my \$cn = \$_[0]; \@_ = ".$code1."\$cn".$code2."; goto \&\$f0; }\n";
    warn "--------- emit_concat: ", join(' ', @elems);
    warn $code;
    warn "--------- ";
    eval($code) || die "$@";
}

sub emit_literal {
    my $elem = shift;
    my $noop = noop();
    warn "emit_literal: $elem";
    if ($elem =~ m[^qr/(.+)/[a-z]*$]) {
        my $regex = eval($elem);
        die "$elem: $@" if $@;
        return sub {
            my $c = $_[0];
            warn "Pattern matching '", substr($X::str, $X::pos), "' against $elem";
            my $s = $X::str;
            pos($s) = $X::pos;
            my $res = $s =~ /\G\s*$regex/;
            return undef if !$res;
            warn "Pattern match OK";
            $X::pos += length($&);
            @_ = $noop;
            goto &$c;
        };
    } else {
        my $len = length($elem);
        return sub {
            my $cn = $_[0];
            #warn Dump($c);
            warn "Exact matching '", substr($X::str, $X::pos), "' against $elem";
            my $s = $X::str;
            pos($s) = $X::pos;
            if ($s =~ /\G\s+/) {
                $X::pos += length($&);
            }
            return undef if !(substr($X::str, $X::pos, $len) eq $elem);
            warn "Exact match OK";
            $X::pos += $len;
            @_ = $noop;
            #$cn = $noop if not defined $cn;
            goto &$cn;
        };
    }
}

sub emit {
    my $elem = shift;
    #warn "emit: $elem";
    if (is_literal($elem)) {
        return emit_literal($elem);
    } else {
        return emit_nonterminal($elem);
    }
}

sub is_literal {
    my $elem = shift;
    if ($elem =~ /^[A-Z][A-Za-z]+[a-z2]$/ and !$rules{$elem}) {
        #warn "Warning: $elem may be an undefined nonterminal.\n";
    }
    return not $rules{$elem};
}

sub parse {
    my($parser, $source) = @_;
    my $len = length($source);
    local $X::str = $source;
    local $X::pos = 0;
    return $parser->($noop);
}

sub compile {
    my $goal = shift;
    return emit_nonterminal($goal);
}

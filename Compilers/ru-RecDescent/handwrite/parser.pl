#: parser.pl

use strict;
use warnings;

package X;

our ($str, $pos, $level);

package Parser;

local $/;
my $src = <>;
die "No input source code.\n" if !defined $src;
print "\n", parse($src) ? 'success' : 'fail', "\n";

sub rulename {
    my $sub = (caller 2)[3];
    $sub =~ s/^\w+:://g;
    $sub;
}

sub try {
    my $rule;
    if (@_) {
        $rule = "'$_[0]'";
    } else {
        $rule = rulename;
    }
    $X::level++;
    my $indent = '  ' x $X::level;
    print "${indent}trying $rule...\n";
}

sub fail {
    my $rule;
    if (@_) {
        $rule = "'$_[0]'";
    } else {
        $rule = rulename;
    }
    my $indent = '  ' x $X::level;
    print "${indent}FAIL to match $rule...\n";
    $X::level--;
}

sub success {
    my $rule;
    if (@_) {
        $rule = "'$_[0]'";
    } else {
        $rule = rulename;
    }
    my $indent = '  ' x $X::level;
    print "${indent}>>MATCH<< $rule...\n";
    $X::level--;
}

sub parse {
    my ($s) = @_;
    $X::str = $s;
    $X::pos = 0;
    $X::level = 0;
    return if_stmt();
}

sub if_stmt {
    try;
    my $commit;
    if (if_stmt_production_1(\$commit)) {
        success;
        return 1;
    }
    return undef if $commit;
    if (if_stmt_production_2(\$commit)) {
        success;
        return 1;
    }
    return undef if $commit;
    undef;
}

sub if_stmt_production_1 {
    my $rcommit = shift;
    try;
    my $saved_pos = $X::pos;
    if (!match_str('if')) {
        fail;
        return undef;
    }
    $$rcommit = 1;
    if (!cond()) {
        $X::pos = $saved_pos;
        fail;
        return undef;
    }
    if (!block()) {
        $X::pos = $saved_pos;
        fail;
        return undef;
    }
    $$rcommit = undef;
    if (!match_str('else')) {
        $X::pos = $saved_pos;
        fail;
        return undef;
    }
    if (!block()) {
        $X::pos = $saved_pos;
        fail;
        return undef;
    }
    success;
    return 1;
}

sub cond {
    try;
    my $match = match_str('cond');
    if ($match) {
        success;
        return 1;
    } else {
        fail;
        return undef;
    }
}

sub block {
    try;
    my $match = match_re('{[^}]*}');
    if ($match) {
        success;
        return 1;
    } else {
        fail;
        return undef;
    }
}

sub if_stmt_production_2 {
    try;
    my $saved_pos = $X::pos;
    if (!match_str('if')) {
        fail;
        return undef;
    }
    if (!cond()) {
        $X::pos = $saved_pos;
        fail;
        return undef;
    }
    if (!block()) {
        $X::pos = $saved_pos;
        fail;
        return undef;
    }
    success;
    return 1;
}

sub match_str {
    my $target = shift;
    try($target);
    my $s = $X::str;
    pos($s) = $X::pos;
    if ($s =~ m/\G\s+/g) {
        $X::pos += length($&);
    }
    #warn substr($s, $X::pos), "\n";
    my $len = length($target);
    my $match = (substr($s, $X::pos, $len) eq $target);
    if (!$match) {
        fail($target);
        return undef;
    }
    $X::pos += $len;
    success($target);
    return 1;
}

sub match_re {
    my $re = shift;
    try($re);
    my $s = $X::str;
    pos($s) = $X::pos;
    if ($s =~ m/\G\s+/g) {
        $X::pos += length($&);
    }
    my $qr = qr/\G(?:$re)/;
    my $match = ($s =~ $qr);
    if (!$match) {
        fail($re);
        return undef;
    }
    $X::pos += length($&);
    success($re);
    return 1;
}

__END__

if_stmt: 'if' <commit> cond block <uncomit> 'else' block
       | 'if' cond block

cond: 'cond'

block: /{[^}]*}/

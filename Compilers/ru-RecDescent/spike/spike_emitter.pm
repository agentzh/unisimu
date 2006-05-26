#: spike_emitter.pm
#: Simple perl code emitter for BNF
#: 2006-05-25 2006-05-25

package Spike::Emitter;

use strict;
use warnings;
use Template;
use Data::Dumper::Simple;

my $TT = Template->new;

sub emit {
    my ($self, $ast, $filetype, $package) = @_;
    #warn Dumper($ast);
    $ast = adjust_ast($ast);
    $ast->{filetype} = $filetype || 'pl';
    $ast->{package} = $package || 'Parser';
    #warn Dumper($ast);
    my $buffer;
    $TT->process(\*DATA, $ast, \$buffer)
        || die $TT->error(), "\n";
    $buffer;
}

sub adjust_ast {
    my $ast = shift;
    my (%altern, %concat, %atoms);
    my $new_ast = {
        startrule   => $ast->{startrule},
        alternation => \%altern,
        concat      => \%concat,
        atoms       => \%atoms,
    };
    my %rules = %{ $ast->{rules} };
    while (my ($rulename, $rprods) = each %rules) {
        my @prods = @$rprods;
        if (@prods == 1) {
            my @items = emit_prod( $prods[0] );
            if (@items > 1) {
                $concat{$rulename} = \@items;
            } else {
                $atoms{$rulename} = $items[0];
            }
        }
        else {
            my @branches;
            for my $i (0..$#prods) {
                my $prodname = "${rulename}_production_" . ($i+1);
                push @branches, $prodname;
                my @items = emit_prod( $prods[$i] );
                if (@items > 1) {
                    $concat{$prodname} = \@items;
                } else {
                    $atoms{$prodname} = $items[0];
                }
            }
            $altern{$rulename} = \@branches;
        }
    }
    $new_ast;
}

sub emit_prod {
    my $prod = shift;
    my @items = @$prod;
    if ($items[0] =~ /^<error/) {
        return ("error()");
    }
    for my $item (@items) {
        if (ref $item) {
            if ($item->[1] eq 's') {
                if ($item->[2]) {
                    if ($item->[2] =~ /^\//) {
                        $item->[2] = "q" . $item->[2];
                    }
                    $item = "repeat_1_n_sep( sub { \&$item->[0] }, $item->[2] )";
                } else {
                    $item = "repeat_1_n( sub { \&$item->[0] } )";
                }
            }
            elsif ($item->[1] eq 's?') {
                if ($item->[2]) {
                    if ($item->[2] =~ /^\//) {
                        $item->[2] = "q" . $item->[2];
                    }
                    $item = "repeat_0_n_sep( sub { \&$item->[0] }, $item->[2] )";
                } else {
                    $item = "repeat_0_n( sub { \&$item->[0] } )";
                }
            }
            elsif ($item->[1] eq '?') {
                $item = "repeat_0_1( sub { \&$item->[0] } )";
            }
            elsif (@$item == 3 and $item->[1] =~ /^\//) {
                $item = "match_leftop( \\\&$item->[0], q$item->[1], \\\&$item->[2] )"
            }
            else {
                die "Unknown modifier $item->[1]\n";
            }
        }
        elsif ($item =~ /^['"]/) {
            $item = "match_str($item)";
        }
        elsif ($item =~ /^\//) {
            $item = "match_re(q$item)";
        }
        elsif ($item =~ /^\w+$/) {
            $item = "\&$item()";
        }
        elsif ($item =~ /^{/) {
            $item = "CORE::eval $item";
        }
    }
    @items;
}

1;
__DATA__
#: parser.pl

package main;

our $RD_TRACE = undef; # default off

package X;

our ($str, $pos, $level);

package [% package %];

use strict;
use warnings;

sub _rulename {
    my $sub = (caller 2)[3];
    $sub =~ s/^\w+:://g;
    $sub;
}

sub _try {
    return if !$::RD_TRACE;
    my $rule;
    if (@_) {
        $rule = shift;
    } else {
        $rule = _rulename;
    }
    $X::level++;
    my $indent = '  ' x $X::level;
    if (!defined $X::saved_pos or $X::saved_pos != $X::pos) {
        my $next = substr($X::str, $X::pos, 15);
        $next =~ s/\n/\\n/g;
        $next =~ s/\t/\\t/g;
        if (length(substr $X::str, $X::pos) > 15) {
            $next .= '...';
        }
        print "${indent}trying $rule...    [$next]\n";
        $X::saved_pos = $X::pos;
    } else {
        print "${indent}trying $rule...\n";
    }
}

sub _fail {
    return if !$::RD_TRACE;
    my $rule;
    if (@_) {
        $rule = shift;
    } else {
        $rule = _rulename;
    }
    my $indent = '  ' x $X::level;
    print "${indent}FAIL to match $rule...\n";
    $X::level--;
}

sub _success {
    return if !$::RD_TRACE;
    my $rule;
    if (@_) {
        $rule = shift;
    } else {
        $rule = _rulename;
    }
    my $indent = '  ' x $X::level;
    print "${indent}>>MATCH<< $rule...\n";
    $X::level--;
}

sub new {
    my $class = shift;
    $class;
}

sub parse {
    my ($self, $text) = @_;
    $X::str = $text;
    $X::pos = 0;
    $X::level = 0;
    return [% startrule %]();
}

[% FOREACH rule = alternation.keys -%]
sub [% rule %] {
    _try;
    my ($match, $commit);
    [%- productions = alternation.$rule %]
    [%- FOREACH production = productions %]
    $match = &[% production %](\$commit);
    if (defined $match) {
        _success;
        return $match;
    }
      [%- IF production != productions.last %]
    return undef if $commit;
      [%- END %]
    [%- END %]
    undef;
}

[% END -%]

[%- FOREACH rule = concat.keys -%]
sub [% rule %] {
    my $rcommit = shift;
    _try;
    my @item = '[% rule %]';
    my $text = $X::str;
    pos($text) = $X::pos;
    my $match;
    my $saved_pos = $X::pos;
  [%- first = 1 %]
  [%- FOREACH atom = concat.$rule %]
    [%- IF atom == '<commit>' %]
    $$rcommit = 1;
    push @item, '<commit>';
    [%- ELSIF atom == '<uncommit>' %]
    $$rcommit = undef;
    push @item, '<uncommit>';
    [%- ELSE %]
    $match = [% atom %];
    if (!defined $match) {
      [%- IF first %]
          [%- first = 0 %]
      [%- ELSE %]
        $X::pos = $saved_pos;
      [%- END %]
        _fail;
        return undef;
    }
    push @item, $match;
    [%- END %]
  [%- END %]
    _success;
    $item[-1];
}

[% END -%]

[%- FOREACH rule = atoms.keys -%]
sub [% rule %] {
    _try;
    my @item = '[% rule %]';
    my $text = $X::str;
    pos($text) = $X::pos;
    my $match = [% atoms.$rule %];
    if (defined $match) {
        _success;
        push @item, $match;
        return $match;
    } else {
        _fail;
        return undef;
    }
}

[% END -%]
sub match_str {
    my $target = shift;
    _try("'$target'");
    my $text = $X::str;
    pos($text) = $X::pos;
    if ($text =~ m/\G\s+/gc) {
        $X::pos += length($&);
    }
    #warn substr($text, $X::pos), "\n";
    my $len = length($target);
    my $equal = (substr($text, $X::pos, $len) eq $target);
    if (!$equal) {
        _fail("'$target'");
        return undef;
    }
    $X::pos += $len;
    _success("'$target'");
    return $target;
}

sub match_re {
    my $re = shift;
    _try("/$re/");
    my $text = $X::str;
    pos($text) = $X::pos;
    if ($text =~ m/\G\s+/gc) {
        $X::pos += length($&);
    }
    if ($re eq "^\\Z") {
        #warn "Matching end of file";
        if ($X::pos == length($X::str)) {
            _success("/$re/");
            return 1;
        }
        _fail("/$re/");
        return undef;
    }
    if ($text !~ /\G(?:$re)/) {
        _fail("/$re/");
        return undef;
    }
    my $match = $&;
    $X::grouping = $1;
    $X::pos += length($&);
    _success("/$re/");
    return $match;
}

sub repeat_1_n_sep {
    my ($coderef, $sep) = @_;
    my @retval;
    my $match = $coderef->();
    if (!defined $match) {
        return undef;
    }
    push @retval, $match;
    while (1) {
        my $saved_pos = $X::pos;
        my $match = match_re($sep);
        last if !defined $match;
        my $sep_match;
        if (defined $X::grouping) {
            $sep_match = $match;
        }
        $match = $coderef->();
        if (!defined $match) {
            $X::pos = $saved_pos;
            last;
        }
        push @retval, $sep_match if defined $sep_match;
        push @retval, $match;
        last if $X::pos == $saved_pos;
    }
    \@retval;
}

sub repeat_1_n {
    my ($coderef) = @_;
    my $match = $coderef->();
    if (!defined $match) {
        return undef;
    }
    my @retval;
    push @retval, $match;
    while (1) {
        my $saved_pos = $X::pos;
        my $match = $coderef->();
        last if !defined $match;
        push @retval, $match;
        last if $X::pos == $saved_pos;
    }
    \@retval;
}

sub repeat_0_n_sep {
    my ($coderef, $sep) = @_;
    my @retval;
    my $match = $coderef->();
    if (!defined $match) {
        return [];
    }
    push @retval, $match;
    while (1) {
        my $saved_pos = $X::pos;
        my $match = match_re($sep);
        last if !defined $match;
        my $sep_match;
        if (defined $X::grouping) {
            $sep_match = $match;
        }
        $match = $coderef->();
        if (!defined $match) {
            $X::pos = $saved_pos;
            last;
        }
        push @retval, $sep_match if defined $sep_match;
        push @retval, $match;
        last if $X::pos == $saved_pos;
    }
    \@retval;
}

sub repeat_0_n {
    my $coderef = $_[0];
    my @retval;
    my $match = $coderef->();
    if (!defined $match) {
        return [];
    }
    push @retval, $match;
    while (1) {
        my $saved_pos = $X::pos;
        my $match = $coderef->();
        if (defined $match) {
            push @retval, $match;
        } else {
            last;
        }
        last if $X::pos == $saved_pos;
    }
    \@retval;
}

sub repeat_0_1 {
    my $coderef = $_[0];
    my $match = $coderef->();
    if (!defined $match) {
        [];
    } else {
        [$match];
    }
}

sub match_leftop {
    my ($sub1, $sep, $sub2) = @_;
    my @retval;
    my $match = $sub1->();
    return undef if !defined $match;
    push @retval, $match;
    while (1) {
        my $saved_pos = $X::pos;
        my $match = match_re($sep);
        last if !defined $match;
        my $sep_match;
        if (defined $X::grouping) {
            $sep_match = $match;
        }
        $match = $sub2->();
        if (!defined $match) {
            $X::pos = $saved_pos;
            last;
        }
        push @retval, $sep_match if defined $sep_match;
        push @retval, $match;
        last if $X::pos == $saved_pos;
    }
    \@retval;
}

sub error {
    warn "Syntax error.\n";
    undef;
}

[%- IF filetype == 'pm' %]
1;
[%- ELSE %]
package main;

use strict;
use warnings;

local $/;
my $src = <>;
die "No input source code.\n" if !defined $src;

$::RD_TRACE = 1;
my $parser = Parser->new;
print "\n", defined($parser->parse($src)) ? 'success' : 'fail', "\n";
[%- END %]

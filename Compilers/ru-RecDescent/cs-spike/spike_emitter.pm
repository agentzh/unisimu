#: spike_emitter.pm
#: Simple perl code emitter for BNF
#: 2006-05-25 2006-05-25

package Spike::Emitter;

use strict;
use warnings;
use Template;
use Data::Dumper::Simple;

my $TT = Template->new;

our $DEBUG;

sub emit {
    my ($self, $ast, $is_module, $package) = @_;
    #warn Dumper($ast);
    $ast = adjust_ast($ast);
    $ast->{is_module} = $is_module;
    $ast->{package} = $package || 'Parser';
    #warn Dumper($ast);
    my $buffer;
    $TT->process(\*DATA, $ast, \$buffer)
        || die $TT->error(), "\n";
    $buffer;
}

sub adjust_preamble {
    my $s = shift;
    return "" if !defined $s;
    $s =~ s/ (\d+)$//s;
    my $lineno = $1;
    $s =~ s/^\{(.*)\}$/$1/g;
    my $directive = $DEBUG ? '' : qq{        #line $lineno "$X::infile"};
    return "$directive\n$s\n";
}

sub adjust_ast {
    my $ast = shift;
    my (%altern, %concat, %atoms);

    my $preamble = adjust_preamble($ast->{preamble});
    my $new_ast = {
        preamble    => $preamble,
        startrule   => $ast->{startrule},
        alternation => \%altern,
        concat      => \%concat,
        atoms       => \%atoms,
    };
    my %rules = %{ $ast->{rules} };
    while (my ($rulename, $rprods) = each %rules) {
        my @prods = @$rprods;
        if (@prods == 1) {
            my @items = emit_prod( $rulename, $prods[0] );
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
                my @items = emit_prod( $rulename, $prods[$i] );
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

# convert perl style q/.../ strings to C-sharp @"..." style strings
sub cs_str {
    my $s = shift;
    my $plain = eval "q$s";
    $plain =~ s/"/""/g;
    qq/"$plain"/;
}

sub emit_prod {
    my ($rulename, $prod) = @_;
    my @items = @$prod;
    if ($items[0] =~ /^<error\?/) {
        return ( qq{error("$rulename")} );
    } elsif ($items[0] =~ /^<error/) {
        return ( qq{error("$rulename")} );
    }
    for my $item (@items) {
        if (ref $item) {
            if ($item->[1] eq 's') {
                if ($item->[2]) {
                    if ($item->[2] =~ /^\//) {
                        $item->[2] = "@" . cs_str($item->[2]);
                    }
                    $item = "repeat_1_n_sep( delegate (ref bool cm) { return $item->[0](ref cm); }, ".
                        "$item->[2], ref commit )";
                } else {
                    $item = "repeat_1_n( delegate (ref bool cm) { return $item->[0](ref cm); }, ref commit )";
                }
            }
            elsif ($item->[1] eq 's?') {
                if ($item->[2]) {
                    if ($item->[2] =~ /^\//) {
                        $item->[2] = '@' . cs_str($item->[2]);
                    }
                    $item = "repeat_0_n_sep( delegate (ref bool cm) { return $item->[0](ref cm); }, ".
                        "$item->[2], ref commit )";
                } else {
                    $item = "repeat_0_n( delegate (ref bool cm) { return $item->[0](ref cm); }, ref commit )";
                }
            }
            elsif ($item->[1] eq '?') {
                $item = "repeat_0_1( delegate (ref bool cm) { return $item->[0](ref cm); }, ref commit )";
            }
            elsif (@$item == 3 and $item->[1] =~ /^\//) {
                $item = "match_leftop( delegate (ref bool cm) { return $item->[0](ref cm); }, ".
                    '@' . cs_str($item->[1]) . ", delegate (ref bool cm) { return $item->[2](ref cm); }, ref commit )";
            }
            else {
                die "Unknown modifier $item->[1]\n";
            }
        }
        elsif ($item =~ /^['"]/) {
            $item = "match_str(@" . cs_str($item) . ")";
        }
        elsif ($item =~ /^\//) {
            $item = 'match_re(@' . cs_str($item) . ")";
        }
        elsif ($item =~ /^\w+$/) {
            $item = "$item(ref commit)";
        }
        elsif ($item =~ /^{/) {
            $item =~ s/ (\d+)$//s;
            my $lineno = $1;
            my $directive = $DEBUG ? '' : qq{        #line $lineno "$X::infile"};
            $item = <<"_EOC_";
XXX
$directive
        PAction action = delegate $item;
        match = action();
        if (match != null && pos > X.pos) X.pos = pos;
_EOC_
        }
    }
    @items;
}

1;
__DATA__
// parser.cs
using System;
using System.Text.RegularExpressions;
using System.Collections;
using Result = System.Object;

// preamble
[% preamble %]

class X {
    public static string str = null;
    public static int pos = 0;
    public static int saved_pos = 0;
    public static int level = 0;
    public static bool group = false;
}

public class Parser {
    public static bool RD_TRACE   = false;  // default off
    public static bool RD_VERBOSE = true;  // default on

    public delegate Result PAction ();
    public delegate Result PParse (ref bool commit);

    private static Regex regex_ws = new Regex(@"(?s)\G\s+");

    private static string str_mul(string s, int times) {
        string res = "";
        for (int i = 0; i < times; i++)
            res += s;
        return res;
    }

    private static void print(string fmt, params object[] args) {
        Console.Write(fmt, args);
    }

    private static void _try (string rule) {
        if (! RD_TRACE) return;
        X.level++;
        string indent = str_mul("  ", X.level);
        if (RD_VERBOSE || X.saved_pos != X.pos) {
            string next = X.str.Substring(X.pos, Math.Min(15, X.str.Length - X.pos));
            next = next.Replace("\r\n", "\\n");
            next = next.Replace("\n", "\\n");
            next = next.Replace("\t", "\\t");
            if (X.str.Length - X.pos > 15)
                next += "...";
            print("{0}trying {1}...    [{2}]\n", indent, rule, next);
            X.saved_pos = X.pos;
        } else {
            print("{0}trying {1}...\n", indent, rule);
        }
    }

    private static void _fail (string rule) {
        if (! RD_TRACE) return;
        string indent = str_mul("  ", X.level);
        print("{0}FAIL to match {1}...\n", indent, rule);
        X.level--;
    }

    private static void _success (string rule) {
        if (!RD_TRACE) return;
        string indent = str_mul("  ", X.level);
        print("{0}>>MATCH<< {1}...\n", indent, rule);
        X.level--;
    }

    public static Result parse (string text) {
        X.str   = text;
        X.pos   = 0;
        X.level = 0;
        bool commit = false;
        return [% startrule %](ref commit);
    }

[% FOREACH rule = alternation.keys -%]
    public static Result [% rule %] (ref bool commit) {
        _try("[% rule %]");
        Result match;
        bool cm = false;
    [%- productions = alternation.$rule %]
    [%- FOREACH production = productions %]
        match = [% production %](ref cm);
        if (match != null) {
            _success("[% rule %]");
            return match;
        }
      [%- IF production != productions.last %]
        if (cm) {
            _fail("[% rule %]");
            return null;
        }
      [%- END %]
    [%- END %]
        _fail("[% rule %]");
        return null;
    }

[% END -%]

[%- FOREACH rule = concat.keys -%]
    public static Result [% rule %] (ref bool commit) {
        _try("[% rule %]");
        ArrayList items = new ArrayList();
        items.Add("[% rule %]");
        string text = X.str;
        int pos = X.pos;
        Result match;
        int saved_pos = X.pos;
    [%- first = 1 %]
    [%- FOREACH atom = concat.$rule %]
      [%- IF atom == '<commit>' %]
        commit = true;
        items.Add("<commit>");
      [%- ELSIF atom == '<uncommit>' %]
        commit = false;
        items.Add("<uncommit>");
      [%- ELSE %]
        // concat
       [%- IF atom.match("^XXX\n") %]
          [%- atom.replace("^XXX", "") %]
       [%- ELSE %]
        match = [% atom %];
       [%- END %]
        if (match == null) {
          [%- IF first %]
              [%- first = 0 %]
          [%- ELSE %]
            X.pos = saved_pos;
          [%- END %]
            _fail("[% rule %]");
            return null;
        }
        items.Add(match);
      [%- END %]
    [%- END %]
        _success("[% rule %]");
        return items[ items.Count - 1 ];
    }

[% END -%]

[%- FOREACH rule = atoms.keys -%]
    public static Result [% rule %] (ref bool commit) {
        _try("[% rule %]");
        ArrayList items = new ArrayList();
        items.Add("[% rule %]");
        string text = X.str;
        int pos = X.pos;
        // atom
        Result match = [% atoms.$rule %];
        if (match != null) {
            _success("[% rule %]");
            items.Add( match );
            return match;
        } else {
            _fail("[% rule %]");
            return null;
        }
    }

[% END -%]
    public static Result match_str (string target) {
        _try(target);
        string text = X.str;
        int pos = X.pos;

        Match m = regex_ws.Match(text, pos);
        if (m.Success) {
            X.pos += m.Length;
        }
        //warn substr($text, $X::pos), "\n";
        int len = target.Length;
        if (X.pos + len > text.Length || text.Substring(X.pos, len) != target) {
            _fail("'" + target + "'");
            return null;
        }
        X.pos += len;
        _success("'" + target + "'");
        return target;
    }

    public static Result match_re (string re) {
        _try("/" + re + "/");
        string text = X.str;
        int pos = X.pos;

        Match m = regex_ws.Match(text, pos);
        if (m.Success) {
            X.pos += m.Length;
        }

        if (re == @"^\Z") {
            //warn "Matching end of file";
            if (X.pos == X.str.Length) {
                _success("/" + re + "/");
                return "";
            }
            _fail("/" + re + "/");
            return null;
        }
        Regex regex = new Regex(@"\G" + re);
        //print("Regex: {0}\n", regex.ToString());
        m = regex.Match(text, X.pos);
        if (! m.Success) {
            _fail("/" + re + "/");
            return null;
        }
        Result match = text.Substring(m.Index, m.Length);
        if (m.Groups.Count > 1) {
            X.group = true;
        } else {
            X.group = false;
        }
        X.pos += m.Length;
        _success("/" + re + "/");
        return match;
    }

    public static Result repeat_1_n_sep (PParse coderef, string sep, ref bool commit) {
        ArrayList retval = new ArrayList();
        Result match = coderef(ref commit);
        if (match == null) return null;
        retval.Add(match);
        while (true) {
            int saved_pos = X.pos;
            match = match_re(sep);
            if (match == null) break;
            Result sep_match = null;
            if (X.group) {
                sep_match = match;
            }
            match = coderef(ref commit);
            if (match == null) {
                X.pos = saved_pos;
                break;
            }
            if (X.pos == saved_pos) break;
            if (sep_match != null) retval.Add(sep_match);
            retval.Add(match);
        }
        return retval;
    }

    public static Result repeat_1_n (PParse coderef, ref bool commit) {
        Result match = coderef(ref commit);
        if (match == null) return null;
        ArrayList retval = new ArrayList();
        retval.Add(match);
        while (true) {
            int saved_pos = X.pos;
            match = coderef(ref commit);
            if (match == null) break;
            if (X.pos == saved_pos) break;
            retval.Add( match );
        }
        return retval;
    }

    public static Result repeat_0_n_sep (PParse coderef, string sep, ref bool commit) {
        ArrayList retval = new ArrayList();
        Result match = coderef(ref commit);
        if (match == null) {
            return retval;
        }
        retval.Add(match);
        while (true) {
            int saved_pos = X.pos;
            match = match_re(sep);
            if (match == null) break;
            Result sep_match = null;
            if (X.group) sep_match = match;
            match = coderef(ref commit);
            if (match == null) {
                X.pos = saved_pos;
                break;
            }
            if (X.pos == saved_pos) break;
            if (sep_match != null) retval.Add(sep_match);
            retval.Add(match);
        }
        return retval;
    }

    public static Result repeat_0_n (PParse coderef, ref bool commit) {
        ArrayList retval = new ArrayList();
        Result match = coderef(ref commit);
        if (match == null) {
            return retval;
        }
        retval.Add(match);
        while (true) {
            int saved_pos = X.pos;
            match = coderef(ref commit);
            if (X.pos == saved_pos) break;
            if (match != null) retval.Add(match);
            else               break;
        }
        return retval;
    }

    public static Result repeat_0_1 (PParse coderef, ref bool commit) {
        Result match = coderef(ref commit);
        ArrayList retval = new ArrayList();
        if (match == null) {
            return retval;
        } else {
            retval.Add(match);
            return retval;
        }
    }

    public static Result match_leftop (PParse sub1, string sep, PParse sub2, ref bool commit) {
        ArrayList retval = new ArrayList();
        Result match = sub1(ref commit);
        if (match == null) return null;
        retval.Add(match);
        while (true) {
            int saved_pos = X.pos;
            match = match_re(sep);
            if (match == null) break;
            Result sep_match = null;
            if (X.group) sep_match = match;
            match = sub2(ref commit);
            if (match == null) {
                X.pos = saved_pos;
                break;
            }
            if (X.pos == saved_pos) break;
            if (sep_match != null) retval.Add(sep_match);
            retval.Add(match);
        }
        return retval;
    }

    private static Result error (string rule) {
        //warn "Syntax error.\n";
        //Console.Error.Write("Syntax error while parsing {0}", rule);
        return null;
    }
}

[%- IF NOT is_module %]

class Runner {
    public static void Main(string[] args) {
        foreach (string opt in args) {
            if (opt == "-d")
                Parser.RD_TRACE = true;
            else if (opt == "-q")
                Parser.RD_VERBOSE = false;
            else
                die("Unknown option " + opt);
        }
        string source = Console.In.ReadToEnd();
        source.Replace("\r\n", "\n");
        if (source == "") die( "No input source code.\n" );
        Result result = [% package %].parse(source);
        if (result == null) {
            die("Syntax error.");
        } else {
            Console.WriteLine(result);
        }
    }
    public static void die(string fmt, params Object[] args) {
        Console.Error.Write(fmt, args);
        System.Environment.Exit(2);
    }
}

/*
my %opts;
getopts('d', \%opts);

local $/;
my $src = <>;
die "No input source code.\n" if !defined $src;

my $parser = [% package %]->new;
my $ast;
if ($opts{d}) {
    $::RD_TRACE = 1;
    $ast = $parser->parse($src);
    print "\n", defined($ast) ? 'success' : 'fail', "\n";
} else {
    $::Data::Dumper::Indent = 1;
    $ast = $parser->parse($src);
    if (!defined $ast) {
        warn "Syntax error.\n";
        exit(1);
    } elsif (ref $ast) {
        print Data::Dumper->Dump([$ast], ['AST']);
    } else {
        print $ast, "\n";
    }
}
*/
[%- END %]

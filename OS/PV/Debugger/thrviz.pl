#: trhviz.pl
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-14 2005-11-14

use strict;
use warnings;

use CGI;
use Data::Dumper;
use Template::Ast;

my $infile = shift;
die "error: No input file specified.\n"
    if !$infile;

open my $in, $infile or
    die "error: Can't open $infile for reading: $!\n";

my $cgi = new CGI;
my $ready = 1;
my $stmt_count = 0;
my (%ast, @stmts, $stmt, $sub_name);
while (<$in>) {
    if ($ready == 1 and s/#-#\s*(.+)$/warn "#-# 0..$1\\n";\n/) {
        $stmt = '';
        $sub_name = $1;
        $ready = 0;
        $stmt_count = 1;
    } elsif ($ready == 0 and s/^(\s*)#-#\s*$/${1}warn "#-# $stmt_count..$sub_name\\n";\n/) {
        push @stmts, quote($stmt);
        $ast{$sub_name} = [@stmts];
        @stmts = ();
        $stmt_count = 0;
        $stmt = '';
        $ready = 1;
    } elsif ($ready == 0) {
        $stmt .= $_;
        if (s/([;{]\s*)(?:#.*)?$/$1 warn "#-# $stmt_count..$sub_name\\n";\n/) {
            $stmt_count++;
            push @stmts, quote($stmt);
            $stmt = '';
        }
    }
} continue {
    print;
}

close $in;

my $astfile = $infile . '.ast';
Template::Ast->write(\%ast, $astfile) or
    die Template::Ast->error;
warn "$astfile generated.\n";

sub quote {
    my $s = shift;
    $s = $cgi->escapeHTML($s);
    $s =~ s/\n/<br>/g;
    $s =~ s/\t/    /g;
    $s =~ s/ /&nbsp;/g;
    return $s;
}

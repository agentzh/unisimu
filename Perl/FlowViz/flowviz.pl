#: flowviz.pl
#: FlowViz compiler that generates FlowAsm code.
#: v0.02
#: Copyright (c) 2005 Agent Zhang
#: 2005-10-29 2005-10-31

use strict;
use warnings;
use Getopt::Std;
use Parse::RecDescent;

my %opts;
getopts('co:', \%opts);
my $infile = shift;
die "Usage: flowviz [-c] [-o <outfile>] <infile>\n" if !$infile;
my $outfile;
if (($outfile = $infile) !~ s/\.fv$/.fa/) {
    $outfile .= '.fa';
}

open my $in, $infile or
    die "error: Can't open $infile for reading: $!\n";
my $src;
{
    local $/;
    $src = <$in>;
}
close $in;

my $grammer = <<'_EOC_';
program: statements

statements: <rulevar: $stmts>

statements: statement(s)                             {  $stmts = $item{'statement(s)'};
                                                        $return = join('', @$stmts); }

statement: if_statement
         | while_statement
		 | label <commit> command					 { $return = "$item{label}\n    $item{command}\n"; }
         | command

if_statement: 'if' <commit> condition
                  block
               <uncommit> 'else'
                  else_block                         { $return = ::process_if_stmt($thisline, %item); }
            | 'if' <commit> condition block          { $return = ::process_if_stmt($thisline, %item); }
            | <error?> <reject>

condition: quoted_text

quoted_text: '(' <commit> /[^)]+/ ')'                { $return = $item{__PATTERN1__}; }
           | '<' <commit> /[^>]+/ '>'                { $return = $item{__PATTERN1__}; }
           | '"' <commit> /[^"]+/ '"'                { $return = $item{__PATTERN1__}; }
           | "'" <commit> /[^']+/ "'"                { $return = $item{__PATTERN1__}; }

block: statement
     | '{' <commit> statements '}'                   { $return = $item{statements}; }

else_block: block

while_statement: 'while' <commit> condition 
                      block                          { $return = ::process_while_stmt($thisline, %item); }
               | <error?> <reject>

command: instruction <commit> operand          { $return = 
                                                        "    $item{instruction} $item{operand}\n"; }
       | <error?> <reject>

label: /(\w+)\s*:/

instruction: 'do' | 'io' | 'start' | 'end' | 'test' | 'jmp' | 'jno' | 'jyes' | 'encoding' | 'font'

operand: quoted_text
       | /[^\n]*/

_EOC_

#$::RD_TRACE = 1;
$::RD_ERRORS = 1;
$::RD_HINT = 1;
#$Parse::RecDescent::skip = '\s+';
#$::RD_AUTOSTUB = 1;
my $parser = Parse::RecDescent->new($grammer) or die "Bad grammar!\n";

my $Counter = 1;
my $asm = $parser->program($src);
defined $asm or die "Process failed due to compilation errors\n";
if ($asm =~ m/\n\s*\w+:[\s\n]*$/s) {
    $asm .= "\n    end End\n";
}

# combine nested lables:
while ($asm =~ s/(_FV_L(\d+):\n[\n\s]*)_FV_L(\d+):\n/$1/os) {
    my ($fir, $sec) = ($2, $3);
    $asm =~ s/_FV_L$sec/_FV_L$fir/g;
}

open my $out, ">$outfile" or
    die "error: Can't open $outfile for writing: $!\n";
print $out $asm;
close $out;
warn "$outfile generated.\n";
if (!$opts{c}) {
    system("flowasm $outfile");
}

sub process_if_stmt {
    my ($lineno, %item) = @_;
    #warn "$Counter";
    my $outer = $Counter + 1;
    my $code;
    if ($item{else_block}) {
        $code = <<_EOC_;
    # line $lineno: if-else statement:
    test $item{condition} ?
    jno _FV_L$Counter
$item{block}
    jmp _FV_L$outer
_FV_L$Counter:
$item{else_block}
_FV_L$outer:
_EOC_
    } else {
        $code = <<_EOC_;
    # line $lineno: if statement:
    test $item{condition} ?
    jno _FV_L$Counter
$item{block}
_FV_L$Counter:
_EOC_
    }
    $Counter = $outer + 1;
    return $code;
}

sub process_while_stmt {
    my ($lineno, %item) = @_;
    $Counter++;
    #warn "$Counter";
    my $outer = $Counter + 1;
    my $code = <<_EOC_;
_FV_L$Counter:
    # line $lineno: while statement:
    test $item{condition} ?
    jno _FV_L$outer
$item{block}
    jmp _FV_L$Counter
_FV_L$outer:
_EOC_
    $Counter = $outer + 1;
    return $code;
}

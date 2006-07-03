#: GraphViz/Flowchart/C.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-07-03 2006-07-03

package GraphViz::Flowchart::C;
use Parse::RecDescent;

use strict;
use Carp qw/croak carp/;
use base 'GraphViz::Flowchart::Base';

my $src;
my $outfile;
my $asm;
my $Counter = 1;

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
                  else_block                         { $return = GraphViz::Flowchart::C::process_if_stmt($thisline, %item); }
            | 'if' <commit> condition block          { $return = GraphViz::Flowchart::C::process_if_stmt($thisline, %item); }
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
                      block                          { $return = GraphViz::Flowchart::C::process_while_stmt($thisline, %item); }
               | <error?> <reject>

command: instruction <commit> operand          { $return = 
                                                        "    $item{instruction} $item{operand}\n"; }
       | <error?> <reject>

label: /(\w+)\s*:/

instruction: 'do' | 'io' | 'start' | 'end' | 'test' | 'jmp' | 'jno' | 'jyes' | 'encoding' | 'font'

operand: quoted_text
       | /[^\n]*/

_EOC_



sub compile_string {
	my $self = shift;
	$src = shift;
	$self->compile();
}

sub compile_file {
	my $self = shift;
	my $infile = shift;
	croak "No input file" if !$infile;
	if (($outfile = $infile) !~ s/\.fv$/.fa/) {
		$outfile .= '.fa';
	}

	open my $in, $infile or
		croak "error: Can't open $infile for reading: $!\n";	
	local $/;
	$src = <$in>;	
	close $in;
	$self->compile();
}

sub compile {
	my $self = shift;

	$::RD_ERRORS = 1;
	$::RD_HINT = 1;
	my $parser = Parse::RecDescent->new($grammer) or croak "Bad grammar!\n";

	$asm = $parser->program($src);
	defined $asm or croak "Process failed due to compilation errors\n";
	if ($asm =~ m/\n\s*\w+:[\s\n]*$/s) {
		$asm .= "\n    end End\n";
	}

	# combine nested lables:
	while ($asm =~ s/(_FV_L(\d+):\n[\n\s]*)_FV_L(\d+):\n/$1/os) {
		my ($fir, $sec) = ($2, $3);
		$asm =~ s/_FV_L$sec/_FV_L$fir/g;
	}

	return 1;

}

sub as_asm {
	shift;
	if($outfile) {
		open my $out, ">$outfile" or
			croak "error: Can't open $outfile for writing: $!\n";
		print $out $asm;
		close $out;
		print  "$outfile generated.";
	} else {
		return $asm;
	}

}

sub process_if_stmt {
    my ($lineno, %item) = @_;
    #carp "$Counter";
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
    #carp "$Counter";
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



1;
__END__

=head1 NAME

GraphViz::Flowchart::C - C-style Language Frontend for GraphViz::Flowchart

=head1 SYNOPSIS

	use GraphViz::Flowchart::C;

	$flowc = GraphViz::Flowchart::C->new;

	# compile source string directly:
	$flowc->compile($flowc_src) or croak $flowc->error();

	# or compile source file
	$flowc->compile_file('foo.fc') or croak $flowc->error();

	# generate PNG-format flowchart:
	$flowc->as_png('foo.png')

	# generate GIF-format flowchart:
	$flowc->as_gif('foo.gif');

	# get the GraphViz object:
	$gv = $flowc->graphviz;
	print $gv->as_ps;  # output PostScript code

	# get pseudo assembly code:
	$asm = $flowc->as_asm;

=head1 DESCRIPTION

=head2 METHODS

=over

=item $obj = GraphViz::Flowchart::C->new

Constructor for this class.

=item $obj->compile($src)

Compiles the source code given by $src. A false value
is returned when there's compilation errors.  You can checkout
the detailed error info via the C<error> method.

=item $obj->compile_file($filename)

Compiles the source file whose name is $filename. A false value
is returned when there's compilation errors. You can checkout
the detailed error info via the C<error> method.

=item $errstr = $obj->error()

Returns the error message when an error occur.

=item $obj->as_png($filename)

=item $pngdata = $obj->as_png()

XXX

=item $obj->as_gif($filename)

=item $pngdata = $obj->as_gif()

XXX

=back

=head1 AUTHOR

Agent Zhang (ียาเดบ)

=head1 COPYRIGHT

Copyright (c) 2006 Agent Zhang. All rights reserved.

=head1 SEE ALSO

L<GraphViz::Flowchart::C::Lanuage>, L<GraphViz::Flowchart::Flowasm>,

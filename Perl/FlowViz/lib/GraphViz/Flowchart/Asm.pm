#: GraphViz/Flowchart/Asm.pm
#: 2006-06-28 2006-06-28

package GraphViz::Flowchart::Asm;

use strict;
use warnings;
use base 'GraphViz::Flowchart::Base';

sub compile {
}

1;
__END__

=head1 NAME

=head1 SYNOPSIS

	use GraphViz::Flowchart::Asm;

	$flowasm = GraphViz::Flowchart::Asm->new;

	$flowasm->compile($flowasm_src);
	# or compile source file

	# or compile source file
	$flowasm->compile_file('foo.fa') or die $flowasm->error();

	# generate PNG-format flowchart:
	$flowasm->as_png('foo.png')

	# generate GIF-format flowchart:
	$flowasm->as_gif('foo.gif');

	# get the GraphViz object:
	$gv = $flowasm->graphviz;
	print $gv->as_ps;  # output PostScript code

	# get pseudo assembly code:
	$asm = $flowasm->as_asm;

=head1 DESCRIPTION

=head1 AUTHOR

Agent Zhang (ียาเดบ)

=head1 COPYRIGHT

Copyright (c) 2006 Agent Zhang. All rights reserved.

=head1 SEE ALSO

L<GraphViz::Flowchart::Flowasm>, L<GraphViz::Flowchart::Flowviz>,
L<GraphViz>

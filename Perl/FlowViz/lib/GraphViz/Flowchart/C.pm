#: GraphViz/Flowchart/C.pm
#: Copyright (c) 2006 Agent Zhang

package GraphViz::Flowchart::C;

use strict;
use warnings;

use base 'GraphViz::Flowchart::Base';

sub compile {
}

sub as_asm {
}

1;
__END__

=head1 NAME

GraphViz::Flowchart::C - C-style Language Frontend for GraphViz::Flowchart

=head1 SYNOPSIS

	use GraphViz::Flowchart::C;

	$flowc = GraphViz::Flowchart::C->new;

	# compile source string directly:
	$flowc->compile($flowc_src) or die $flowc->error();

	# or compile source file
	$flowc->compile_file('foo.fc') or die $flowc->error();

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

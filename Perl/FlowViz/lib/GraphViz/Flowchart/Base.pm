#: GraphViz/Flowchart/Base.pm
#: 2006-06-28 2006-06-28

package GraphViz::Flowchart::Base;

use strict;
use warnings;

sub new {
	my $class = shift;
	$class = ref $class || $class;
	return bless {}, $class;
}

sub compile_file {
}

sub as_png {
}

sub as_gif {
}

sub graphviz {
}

1;
__END__

=head1 NAME

GraphViz::Flowchart::Base - Base Class for All the GraphViz::Flowchart Frontends


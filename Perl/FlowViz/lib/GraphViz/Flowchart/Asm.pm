#: GraphViz/Flowchart/Asm.pm
#: 2006-06-28 2006-07-02

package GraphViz::Flowchart::Asm;
use base GraphViz::Flowchart::Base;


use strict;
use warnings;
use GraphViz;
use Encode 'decode';
use Carp qw/carp croak/;


my %shapes = (
    start => 'ellipse',
    do => 'box',
    test => 'diamond',
    io => 'parallelogram',
    end => 'ellipse',
);

my %edges;
my $gv;
my ($outfile, $infile);
my $prev = { name => '', cond => '' };
my ($encoding, $font, $width, $height);


sub compile {
	my $self = shift;
	$infile = shift || croak "Usage: flowasm <infile>\n";	
	
	if (!$outfile) {
		$outfile = $infile;
		if ($outfile !~ s/\.fa$/.png/i) {
			$outfile .= '.png';
		}
	}
	open my $in, $infile or croak "error: Can't open $infile for reading: $!\n";
	#carp "infile: $infile\n";
	#carp "outfile: $outfile\n";

	my $label = '_line0';
	while (<$in>) {
		chomp;
		next if /^\s*$/ or /^\s*#/;
		if (/^\s*encoding\s+(\S+)/o) {
			$encoding = $1;
		}
		elsif (/^\s*font\s+(\S+)/o) {
			$font = $1;
		}
		elsif (/^\s*width\s+(\S+)/o) {
			$width = $1;
		}
		elsif (/^\s*height\s+(\S+)/o) {
			$height = $1;
		}
		elsif (s/^\s*(\w+)\s*://og) {
			$label = $1;
			redo;
		}
		elsif (/^\s*(\S+)\s*(.*)$/o) {
			$self->process_ins($label, $1, $2) || return 0;
			$label = "_line$.";
		}
		else {
			carp "syntax error: $infile: line $.: $_\n";
			return 0;
		}
	}

	close $in;
	return 1;
}

sub process_ins {
	my $self = shift;
    my ($label, $op, $txt) = @_;
    $txt =~ s/([^\\])\\n/$1\n/go;
    $txt = decode($encoding, $txt) if $encoding;
    $op = lc($op);
    $gv ||= $self->new_gv();
    if ($op eq 'jno') {
        $self->add_edge($prev->{name} => $txt, ' No ');
        $prev->{cond} = ' Yes ';
        return 1 ;
    } elsif ($op eq 'jyes') {
        $self->add_edge($prev->{name} => $txt, ' Yes ');
        $prev->{cond} = ' No ';
        return 1;
    } elsif ($op eq 'jmp') {
        $self->add_edge($prev->{name} => $txt) if $prev->{name} and $txt;
        $prev->{name} = '';
        $prev->{cond} = '';
        return 1;
    } else {}

    if ($op eq 'end' and !$txt) {
        $prev->{name} = '';
        $prev->{cond} = '';
        return 1;
    }

    my $shape = $shapes{$op};
    if ($shape) {
        $gv->add_node($label, label => $txt, shape => $shape, fontname => $font);
        $self->add_edge($prev->{name} => $label, $prev->{cond})
            if $prev->{name} and $op ne 'start';
        $prev->{name} = $op eq 'end' ? '' : $label;
        $prev->{cond} = '';
    } else {
		
        carp "syntax error: $infile: line $.: unknown instruction \"$op\"\n";
		return;
    }
	return 1;
}

sub new_gv {
    my %args = (
        node => {
            fillcolor => '#f1e1f4',
            color => '#918194',
            style => 'filled',
            fontsize => 10,
        },
        edge => {
            color => 'red',
            fontsize => 10,
        },
    );
    if ($width) { $args{width} = $width }
    if ($height) { $args{height} = $height }
    return GraphViz->new(%args);
}

sub add_edge {
	shift;
    my ($a, $b, $cond) = @_;
    return if $edges{"$a:$b"};
    $gv->add_edge($a => $b, label => $cond, fontname => $font);
    $edges{"$a:$b"} = 1;
}


sub as_png {
	shift;
	croak "carping: flowchart is empty!\n" if !$gv;
	$gv->as_png($outfile);
}

sub as_gif {
	shift;
	croak "carping: flowchart is empty!\n" if !$gv;
	$gv->as_gif($outfile);
}

sub graphviz {
	shift;
	return $gv;
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
	$flowasm->compile_file('foo.fa') or croak $flowasm->error();

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

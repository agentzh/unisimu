#: flowasm.pl
#: FlowAsm compiler that generates flowcharts in PNG format
#: v0.01
#: Copyright (c) 2005 Agent Zhang
#: 2005-10-28 2005-10-30

use strict;
use warnings;

use GraphViz;
use Encode 'decode';
use Getopt::Std;

my %opts;
getopts('o:', \%opts);

my $infile = shift;
$infile || die "Usage: flowasm <infile>\n";
my $outfile = $opts{o};
if (!$outfile) {
    $outfile = $infile;
    if ($outfile !~ s/\.fa$/.png/i) {
        $outfile .= '.png';
    }
}

warn "infile: $infile\n";
warn "outfile: $outfile\n";

open my $in, $infile or
    die "error: Can't open $infile for reading: $!\n";

my %shapes = (
    start => 'ellipse',
    do => 'box',
    test => 'diamond',
    io => 'parallelogram',
    end => 'ellipse',
);

my %edges;
my $gv;
my ($encoding, $font, $width, $height);
my $label = '_line0';
my $prev = { name => '', cond => '' };
while (<$in>) {
    chomp;
    next if /^\s*$/;
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
        process_ins($label, $1, $2);
        $label = "_line$.";
    }
    else {
        die "syntax error: $infile: line $.: $_\n";
    }
}

close $in;

die "warning: flowchart is empty!\n" if !$gv;
$gv->as_png($outfile);

sub process_ins {
    my ($label, $op, $txt) = @_;
    $txt =~ s/([^\\])\\n/$1\n/go;
    $txt = decode($encoding, $txt) if $encoding;
    $op = lc($op);
    $gv ||= new_gv();
    if ($op eq 'jno') {
        add_edge($prev->{name} => $txt, ' No ');
        $prev->{cond} = ' Yes ';
        return;
    } elsif ($op eq 'jyes') {
        add_edge($prev->{name} => $txt, ' Yes ');
        $prev->{cond} = ' No ';
        return;
    } elsif ($op eq 'jmp') {
        add_edge($prev->{name} => $txt) if $prev->{name} and $txt;
        $prev->{name} = '';
        $prev->{cond} = '';
        return;
    }

    if ($op eq 'end' and !$txt) {
        $prev->{name} = '';
        $prev->{cond} = '';
        return;
    }

    my $shape = $shapes{$op};
    if ($shape) {
        $gv->add_node($label, label => $txt, shape => $shape, fontname => $font);
        add_edge($prev->{name} => $label, $prev->{cond})
            if $prev->{name} and $op ne 'start';
        $prev->{name} = $op eq 'end' ? '' : $label;
        $prev->{cond} = '';
    } else {
        die "syntax error: $infile: line $.: unknown instruction \"$op\"\n";
    }
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
    my ($a, $b, $cond) = @_;
    return if $edges{"$a:$b"};
    $gv->add_edge($a => $b, label => $cond, fontname => $font);
    $edges{"$a:$b"} = 1;
}

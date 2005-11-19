#: reasonviz.pl
#: Reasonviz compiler that generates PNG graphs
#: v0.01
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-13 2005-11-13

use strict;
use warnings;
use GraphViz;
use Getopt::Std;

my %opts;
getopts('o:', \%opts);

my $counter = 0;

my $infile = shift;
$infile || die "Usage: flowasm <infile>\n";
my $outfile = $opts{o};
if (!$outfile) {
    $outfile = $infile;
    if ($outfile !~ s/\.rv$/.png/i) {
        $outfile .= '.png';
    }
}

warn "infile: $infile\n";
warn "outfile: $outfile\n";

my %args = (
    #node => {
        #color => '',
        #shape => 'plaintext',
        #fontsize => 10,
    #},
    edge => {
        color => 'red',
        fontsize => 10,
    },
);
my $gv = GraphViz->new(%args);

open my $in, $infile or
    die "error: Can't open $infile for reading: $!\n";

my (@prev, $pngfile);
while (<$in>) {
    chomp;
    next if /^\s*$/;
    if (/^given\s+(.*)/) {
        $pngfile = gen_png($1);
        my $node = "N$counter";
        $gv->add_node($node, shapefile => $pngfile, label => '');
        @prev = ($node);
    } elsif (/^but\s+(.*)/) {
        $pngfile = gen_png($1);
        my $node = "N$counter";
        $gv->add_node($node, shapefile => $pngfile, label => '');
        push @prev, $node;
    } elsif (/^then\s+(.*)/) {
        $pngfile = gen_png($1);
        $gv->add_node("N$counter", shapefile => $pngfile, label => '');
    }
}
close $in;

$gv->as_png($outfile);
warn "$outfile generated.\n";

sub gen_png {
    my $s = shift;
    my $tmp = "tmp" . $counter++;
    #return "$tmp.png" if "$tmp.png";
    
    unlink "tmp.tex";
    unlink "tmp.pdf";
    unlink "tmp.png";
    
    open my $out, ">$tmp.tex" or
        die "error: Can't open $tmp.tex for reading: $!\n";
    my $latex = <<_EOC_;
\\documentclass{article}
\\pagestyle{empty}
\\pdfoutput=1
\\usepackage{amsmath}
\\begin{document}
$s
\\end{document}
_EOC_
    print $out $latex;
    close $out;
    if (system("pdflatex $tmp.tex") != 0) {
        die "Conversion from LaTex to PDF failed.\n".
            "    (Make sure pdflatex is ready.)\n";
    }
    if (system("convert -trim -density 200x200 -border 5 -bordercolor white $tmp.pdf $tmp.png") != 0) {
        die "Conversion from PDF to PNG failed.\n".
            "    (Make sure convert is ready.)\n";
    }
    if (!-e "$tmp.png") {
        die "error: $tmp.png not found.\n";
    }
    return "$tmp.png";
}

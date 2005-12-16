#: reasonviz.pl
#: Reasonviz compiler that generates PNG graphs
#: v0.01
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-13 2005-11-19

use strict;
use warnings;

use File::Spec;
use Latex::Image;
use GraphViz;
use Getopt::Std;
use Switch 'Perl6';

my %opts;
getopts('o:d:e:f:t:ch', \%opts);

if ($opts{h}) {
    print <<'_EOC_';
usage: reasonviz [-f <num>] [-t <num>] [-o <outfile>] 
    [-d <num>x<num>] [-e <encoding>] [-h] <infile>

options:
    -f <num>       Skip the node picture generations
                     before <num> (the base is 0).
    -t <num>       Skip the node picture generations
                     after <num> (the base is 0).
    -o <outfile>   Specify the output file explicitly.
    -d <num>x<num> Specify the density for the node
                     pictures (default to 150x150).
    -e <encoding>  Specify the encoding used by the
                     plain text (not LaTeX code).
    -h             Print this help to stdout.
    -c             Open CJK support

Report bugs to Agent Zhang <agent2002@126.com>.
_EOC_
    exit(0)
}

my $density = $opts{d} || '150x150';
my $encoding = $opts{e};
my $from = $opts{f} || 0;
my $to = $opts{t};
my $cn = $opts{c};

my $header = <<'.';
\documentclass{article}
\usepackage{amsmath}
\usepackage{CJK}
\begin{document}
\begin{CJK*}{GBK}{song}
.

my $footer = <<'.';
\end{CJK*}
\end{document}
.

my $counter = 0;

my $infile = shift or
    die "error: No input file specified.\n",
        "  Use -h option to see the usage.\n";
my $outfile = $opts{o};
if (!$outfile) {
    $outfile = $infile;
    if ($outfile !~ s/\.rv$/.png/i) {
        $outfile .= '.png';
    }
}

#warn "input:  $infile\n";
#warn "output: $outfile\n";

my %args = (
    node => {
        color => 'blue',
        shape => 'box',
        #fontsize => 10,
    },
    edge => {
        color => 'red',
        #fontsize => 10,
    },
);
my $gv = GraphViz->new(%args);
my $tmpdir = File::Spec->tmpdir;

open my $in, $infile or
    die "error: Can't open $infile for reading: $!\n";

my @pngfiles;
my @hypos;
my $prev = { type => '', name => '' };
my $node_id = 0;
my $png_id  = 0;
my ($cmd, $body, $name);
my $ready = 1;
while (<$in>) {
    chomp;
    next if /^\s*$/ or /^\s*#/;
    if ($ready and /^\s*(\w+)\s+(.+)/) {
        ($cmd, $body) = ($1, $2);
        if ($body =~ s/\s+_$//) {
            $ready = 0;
            next;
        }
        if ($body =~ s/\s*\((\d+)\)\s*$//) {
            $name = "n$1";
        } else {
            $name = "node".$node_id++;
        }
        process_cmd($cmd, $body, $name);
        $prev->{name} = $name;
        $prev->{type} = $cmd;
    } elsif (not $ready) {
        s/^\s+/ /;
        if (s/\s+_$//) {
            $body .= "\n$_";
        } else {
            $body .= "\n$_";
            if ($body =~ s/\s*\((\d+)\)\s*$//) {
                $name = "n$1";
            } else {
                $name = "node".$node_id++;
            }
            process_cmd($cmd, $body, $name);
            $prev->{name} = $name;
            $prev->{type} = $cmd;
            $ready = 1;
        }
    } else {
        error("Unrecognized line");
    }
}
close $in;

gen_png_files();

$gv->as_png($outfile);
warn "info: $outfile generated.\n";

sub process_cmd {
    my ($cmd, $body, $name) = @_;
    given ($cmd) {
        when 'given' {
            if (not $prev->{type} or $prev->{type} eq 'then') {
                @hypos = $name;
            } else {
                follow_error('given', $prev->{type});
            }
        }
        when 'and' {
            if (not $prev->{type}) {
                first_line_error('and');
            } elsif ($prev->{type} eq 'then') {
                follow_error('and', 'then');
            } else {
                push @hypos, $name;
            }
        }
        when 'but' {
            if (not $prev->{type}) {
                first_line_error('but');
            } elsif ($prev->{type} eq 'then') {
                @hypos = ($prev->{name}, $name);
            } else {
                follow_error('but', $prev->{type});
            }
        }
        when 'then' {
            if (not $prev->{type}) {
                first_line_error('then');
            } elsif ($prev->{type} eq 'then') {
                @hypos = $prev->{name};
                draw_edges(@hypos => $name);
            } else {
                draw_edges(@hypos => $name);
            }
        }
        default {
            error("Unrecognized command ``$cmd''");
        }
    }
    draw_node($name, $body) if $body;
}

sub error {
    die "error: line $.: @_.\n";
}

sub first_line_error {
    error("``$_[0]'' can't be the first command");
}

sub follow_error {
    error("``$_[0]'' can't follow command ``$_[1]''");
}

sub draw_edges {
    my $child = pop;
    my @parents = @_;
    foreach (@parents) {
        $gv->add_edge($_ => $child);
    }
}

sub draw_node {
    my ($name, $label) = @_;
    $label = decode($encoding) if $encoding;
    if ($label =~ /\$.*\$/s or $label =~ /\\\[.*\\\]/s) {
        my $pngfile = gen_png_name($label);
        $gv->add_node($name, label => '', shapefile => $pngfile);
    } else {
        $label =~ s/\\n/\n/g;
        $gv->add_node($name, label => $label);
    }
}

sub gen_png_name {
    my $latex = shift;
    my $pngfile = File::Spec->catfile($tmpdir, "tmp".$png_id++.".png");
    push @pngfiles, $latex, $pngfile;
    return $pngfile;
}

sub gen_png_files {
    my $i = 0;
    while (@pngfiles) {
        my $latex = shift @pngfiles;
        my $pngfile = shift @pngfiles;
        next if $i < $from;
        last if defined $to and $i > $to;
        print "  index: $i\n";
        Latex::Image->convert(
            $latex,
            $pngfile,
            density => $density,
            header => $header,
            footer => $footer
        );
    } continue {
        $i++;
    }
}

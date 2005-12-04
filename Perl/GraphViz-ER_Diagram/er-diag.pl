#: er-diag.pl
#: E-R Diagram generator based on Graphviz
#: v0.01
#: Copyright (c) 2005 Agent Zhang
#: 2005-12-03 2005-12-03

use strict;
use warnings;

#BEGIN {
#    binmode(\*STDOUT, ":utf8");
#    binmode(\*STDERR, ":utf8");
#}

use GraphViz;
use Getopt::Std;
use encoding 'GB2312';
use Encode qw/decode/;

my %opts;
getopts('o:f:trh', \%opts);

if ($opts{h}) {
    print <<'_EOC_';
usage: er-diagram [-o <outfile>] [-e <encoding>] [-h]
    [-f <font>] <infile>

options:
    -o <outfile>   Specify the output file explicitly.
    -e <encoding>  Specify the encoding used by the
                     plain text (not LaTeX code).
    -f <font>      Font I use
    -h             Print this help to stdout.
    -t             No properties (trim mode)
    -r             Relation only

Report bugs to Agent Zhang <agent2002@126.com>.
_EOC_
    exit(0)
}

my $encoding = $opts{e} || 'GB2312';
my $font = $opts{f} || 'simsun.ttc';
my $trim = $opts{t};
my $rel_only = $opts{r};
my $infile = shift or
    die "error: No input file specified.\n",
        "  Use -h option to see the usage.\n";
my $outfile = $opts{o};
if (!$outfile) {
    $outfile = $infile;
    if ($outfile !~ s/\.cm$/.png/i) {
        $outfile .= '.png';
    }
}

my %EntityStyle =
(
    shape => 'box',
    style => 'filled',
    fillcolor => '#f5f694',
    fontname => $font,
);

my %PropStyle =
(
    shape => 'ellipse',
    style => 'filled',
    fillcolor => '#B8EFEE',
    fontname => $font,
);

my %EdgeStyle =
(
    color => 'red',
);

my %RelationStyle =
(
    shape => 'diamond',
    style => 'filled',
    fillcolor => '#c7f75c',
    fontname => $font,
);

my %InitArgs = (
    layout => 'neato',
    ratio => 'auto',
    #no_overlap => 1,
    directed => 0,
    height => 40,
    width => 40,
    pageheight => 100,
    pageheight => 100,
    node => \%EntityStyle,
    edge => \%EdgeStyle,
);

#$InitArgs{layout} = 'fdp' if $rel_only;
if (not $rel_only and not $trim) {
    #undef $InitArgs{height};
    #undef $InitArgs{width};
}
$InitArgs{no_overlap} = 1 if $rel_only;

our %Nodes;

open my $in, $infile or
	die "error: Can't open $infile for reading: $!\n";

my $gv = GraphViz->new(%InitArgs);

my $idpat = qr/[\w\|]+/x;
my $idpat2 = qr/[\w\|]+ | \[ [\w\|]+ \]/x;

my $id = 1;
my %entity_ids;
while (<$in>) {
    $_ = adjust($_);
    next if /^\s*$/;
    if (/^ \s* ($idpat) \s*
          \( \s* ( $idpat2 (?: \s* , \s* $idpat2 )* ) \s* \) \s* $/x) {
        #print "Match entity($.): $1 ==>\n";
        my ($entity, $list) = ($1, $2);
        my @props = split( /\s*,\s*/, $list );
        #print "@props\n";
        # add Entity node
        (my $key = $entity) =~ s/^\[|\]$//g;
        #print "KEY = $key\n\n";
        my $entity_id;
        if (!exists $entity_ids{$key}) {
            $entity_ids{$key} = gen_id();
        }
        $entity_id = $entity_ids{$key};
        $gv->add_node($entity_id, label => $entity, %EntityStyle);
        foreach (@props) {
            my $prop = gen_id();
            # add property node
            $gv->add_node($prop, label => $_, %PropStyle) if not $rel_only;
            $gv->add_edge($prop => $entity_id) if not $rel_only;
        }
    }
    elsif (/^ \s* ( $idpat (?: \s* , \s* $idpat )* ) \s+ ISA \s+ ($idpat) \s* $/x) {
        #print "Match ISA($.): $1 ==>\n";
        my ($list, $parent) = ($1, $2);
        my @children = split(/\s*,\s*/, $list);
        #print "@children\n";
        my $parent_id = $entity_ids{$parent} or
                error("superclass $parent not found");
        foreach my $child (@children) {
            my $child_id = gen_id();
            $entity_ids{$child} = $child_id;
            $gv->add_node($child_id, label => "| $child |", %EntityStyle);
            $gv->add_edge($child_id => $parent_id, label => 'ISA') if not $trim;
        }
    }
    elsif (/^ \s* ($idpat) \s+
              ($idpat \s* : \s* $idpat (?: \s* : \s* $idpat )*) \s* =
              \s* (\w+ \s* : \s* \w+ (?: \s* : \s* \w+ )*) \s* $/x) {
        #print "Match Relationshifp($.): $1 ==>\n";
        my ($rel, $list1, $list2) = ($1, $2, $3);
        my @entities = split /\s*:\s*/, $list1;
        my @scales = split /\s*:\s*/, $list2;
        #print "@entities, @scales\n";
        my $rel_id = gen_id();
        $gv->add_node($rel_id, label => $rel, %RelationStyle);
        my $i = 0;
        foreach my $entity (@entities) {
            my $entity_id = $entity_ids{$entity} or
                error("entity $entity not found");
            $gv->add_edge($rel_id => $entity_id, label => $scales[$i]) if not $trim;
            $i++;
        }
    }
    else {
        print "Syntax error($.): $_";
        exit(1);
    }
}

if ($gv->as_png($outfile)) {
    print "$outfile generated.\n";
}

sub adjust {
    local $_ = shift;
    $_ = decode($encoding, $_);
    s/£¨/(/g;
    s/£©/)/g;
    s/£º/:/g;
    s/£¬/,/g;
    s/¡¡/ /g;
    return $_;
}

sub gen_id {
    return "node".$id++;
}

sub error {
    my $msg = shift;
    print "error ($.): $msg\n";
    exit(1);
}

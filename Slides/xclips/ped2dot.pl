use strict;
use warnings;

use Getopt::Std;
use Data::Dump::Streamer;
use Template;

my %opts;
getopts('s', \%opts);
my $strict = $opts{s};

my (%males, %person, @families);
while (<>) {
    next if /^\s*$/;
    if (/(.*\S)\s*:\s*(.*\S)\s*$/) {
        my ($parents, $children) = ($1, $2);

        my @parent = split /\s*,\s*/, $parents;
        for (@parent) { $person{$_} = $. }

        my @child = split /\s*,\s*/, $children;
        for (@child) { $person{$_} = $. }
        
        push @families, [\@parent, \@child];
    }
    elsif (/^\s*M\s+(.*\S)\s*$/) {
        my @males = split /\s*,\s*/, $1;
        for (@males) { $males{$_} = 1 }
    }
    else {
        die "syntax error: $_";
    }
}

my @females;
for (keys %person) {
    if (!$males{$_}) { push @females, $_ }
}
my @males = sort keys %males;
@females = sort @females;

my $data = {
    strict   => $strict,
    males    => \@males,
    females  => \@females,
    families => \@families,
};
#print Dump($data)->Out;

my $dot_template = <<'_EOC_';
digraph aGraph {
    fontname="simsun.ttc";
    center=true;
    concentrate=true;
    edge [dir=none,color=red];

    /* normal female */
    node [shape=ellipse, style=filled, fillcolor="#f1e1f4"];
  [%- FOREACH female = females %]
    "[% female %]" [fontname="simsun.ttc", label="[% female %]"
    [%- IF female.match('\?') %],label=""[%- ELES %][% END %] ];
  [%- END %]

    /* normal male */
    node [shape=box];
  [%- FOREACH male = males %]
    "[% male %]" [fontname="simsun.ttc", label="[% male %]"];
  [%- END %]

    /* internal nodes */
    node [shape=none,style=filled,color=red,label="",height=0,width=0];

  [%- FOREACH family = families %]
    [%- parents  = family.0 %]
    [%- children = family.1 %]
    /* spouses */
    [%- p_mid = parents.0 _ "_X_" _ parents.1 %]
    { rank=same; "[% parents.0 %]" -> "[% p_mid %]" -> "[% parents.1 %]"; }

    [%- IF children.size == 1 %]
    "[% p_mid %]" -> "[% children.0 %]"
      [%- NEXT %]
    [%- END %]

    /* parent-child */
    [%- c_mid_i = children.size / 2 %]
    [%- c_mid_i = c_mid_i % 10000000 %];
    "[% p_mid %]" -> "X_[% children.$c_mid_i %]";
    [%- points = [] %]
    [%- FOREACH child = children %]
      [%- IF strict %]
        [%- point = '"X_' _ child _ '"' %]
    [% point %] -> "[% child %]";
      [%- points.push(point) %]
      [%- ELSE %]
    "X_[% children.$c_mid_i %]" -> "[% child %]"
      [%- END %]
    [%- END %]

    [%- IF ! strict %]
    /* siblings */
    { rank=same;
      [% points.join(" ->\n      ", points) %]
    }
    [%- ELSE %]
    [%- END %]

  [%- END %]
}
_EOC_

my $tt = Template->new;

$tt->process(\$dot_template, $data)
    || die $tt->error();

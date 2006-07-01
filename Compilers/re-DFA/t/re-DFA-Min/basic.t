# basic.t

use Test::Base;
use FindBin;
use re::DFA::Min;

my $Debug = 0;

plan tests => 2 * blocks();

run {
    my $block = shift;
    my $regex = $block->regex;
    my $name = $block->name;
    my $graph = re::DFA::Min->transform($regex)->normalize;
    ok $graph, "$name - $regex - re::Graph obj ok";
    my $gv = $graph->as_graphviz;
    my $got = $gv->as_debug;
    my $expected = $block->dot;
    $got =~ s/^\s+/    /gm;
    #$got =~ s/label=" (\x{3b5}) "/label=" eps "/g;
    #$expected =~ s/\s+//g;
    is $got, $expected, "$name - $regex - dot output ok";
    if ($Debug) {
        my $name = $block->name;
        $name =~ s/\W+/_/g;
        $name =~ s/^_+|_+$//g;
        $gv->as_png($FindBin::Bin . '/' . $name . '.png');
    }
};

__DATA__

=== TEST 1:
--- regex: a
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="1"];
    node2 [label="2", shape="doublecircle"];
    node3 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node3 -> node1;
    node1 -> node2 [label=" a "];
}



=== TEST 2:
--- regex: ab
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="1"];
    node2 [label="2"];
    node3 [label="3", shape="doublecircle"];
    node4 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node4 -> node1;
    node1 -> node2 [label=" a "];
    node2 -> node3 [label=" b "];
}



=== TEST 3:
--- regex: a*
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="1", shape="doublecircle"];
    node2 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node2 -> node1;
    node1 -> node1 [label=" a "];
}



=== TEST 4:
--- regex: a|b
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="1"];
    node2 [label="2", shape="doublecircle"];
    node3 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node3 -> node1;
    node1 -> node2 [label=" a "];
    node1 -> node2 [label=" b "];
}



=== TEST 5:
--- regex: (a|b)*(aa|bb)(a|b)*
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="4", shape="doublecircle"];
    node2 [label="1"];
    node3 [label="2"];
    node4 [label="3"];
    node5 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node5 -> node2;
    node2 -> node3 [label=" a "];
    node2 -> node4 [label=" b "];
    node3 -> node4 [label=" b "];
    node3 -> node1 [label=" a "];
    node4 -> node3 [label=" a "];
    node4 -> node1 [label=" b "];
    node1 -> node1 [label=" a "];
    node1 -> node1 [label=" b "];
}



=== TEST 6:
--- regex: (a|)b*
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="1", shape="doublecircle"];
    node2 [label="2", shape="doublecircle"];
    node3 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node3 -> node1;
    node1 -> node2 [label=" a "];
    node1 -> node2 [label=" b "];
    node2 -> node2 [label=" b "];
}



=== TEST 7:
--- regex:
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node2 [label="1", shape="doublecircle"];
    node1 -> node2;
}



=== TEST 8:
--- regex: (a|ba)*
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="1", shape="doublecircle"];
    node2 [label="2"];
    node3 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node3 -> node1;
    node1 -> node1 [label=" a "];
    node1 -> node2 [label=" b "];
    node2 -> node1 [label=" a "];
}



=== TEST 9:
--- regex: a|b|c|d
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="1"];
    node2 [label="2", shape="doublecircle"];
    node3 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node3 -> node1;
    node1 -> node2 [label=" a "];
    node1 -> node2 [label=" b "];
    node1 -> node2 [label=" c "];
    node1 -> node2 [label=" d "];
}

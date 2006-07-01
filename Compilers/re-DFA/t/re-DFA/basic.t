# basic.t

use Test::Base;
use FindBin;
use re::DFA;

my $Debug = 0;

plan tests => 2 * blocks();

run {
    my $block = shift;
    my $regex = $block->regex;
    my $graph = re::DFA->transform($regex)->normalize;
    ok $graph, "$regex - re::Graph obj ok";
    my $gv = $graph->as_graphviz;
    my $got = $gv->as_debug;
    my $expected = $block->dot;
    $got =~ s/^\s+/    /gm;
    #$got =~ s/label=" (\x{3b5}) "/label=" eps "/g;
    #$expected =~ s/\s+//g;
    is $got, $expected, "$regex - dot output ok";
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
    node2 [label="2", shape="doublecircle"];
    node3 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node3 -> node1;
    node1 -> node2 [label=" a "];
    node2 -> node2 [label=" a "];
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
    node3 [label="3", shape="doublecircle"];
    node4 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node4 -> node1;
    node1 -> node2 [label=" a "];
    node1 -> node3 [label=" b "];
}



=== TEST 5:
--- regex: (a|b)*(aa|bb)(a|b)*
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="6", shape="doublecircle"];
    node2 [label="7", shape="doublecircle"];
    node3 [label="3"];
    node4 [label="2"];
    node5 [label="5", shape="doublecircle"];
    node6 [label="8", shape="doublecircle"];
    node7 [label="9", shape="doublecircle"];
    node8 [label="4", shape="doublecircle"];
    node9 [label="1"];
    node10 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node10 -> node9;
    node9 -> node4 [label=" a "];
    node9 -> node3 [label=" b "];
    node4 -> node3 [label=" b "];
    node4 -> node8 [label=" a "];
    node3 -> node4 [label=" a "];
    node3 -> node5 [label=" b "];
    node8 -> node1 [label=" a "];
    node8 -> node2 [label=" b "];
    node5 -> node6 [label=" a "];
    node5 -> node7 [label=" b "];
    node1 -> node1 [label=" a "];
    node1 -> node2 [label=" b "];
    node2 -> node6 [label=" a "];
    node2 -> node7 [label=" b "];
    node6 -> node1 [label=" a "];
    node6 -> node2 [label=" b "];
    node7 -> node6 [label=" a "];
    node7 -> node7 [label=" b "];
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
    node3 [label="3", shape="doublecircle"];
    node4 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node4 -> node1;
    node1 -> node2 [label=" a "];
    node1 -> node3 [label=" b "];
    node2 -> node3 [label=" b "];
    node3 -> node3 [label=" b "];
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
    node1 [label="4", shape="doublecircle"];
    node2 [label="2", shape="doublecircle"];
    node3 [label="3"];
    node4 [label="1", shape="doublecircle"];
    node5 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node5 -> node4;
    node4 -> node2 [label=" a "];
    node4 -> node3 [label=" b "];
    node2 -> node2 [label=" a "];
    node2 -> node3 [label=" b "];
    node3 -> node1 [label=" a "];
    node1 -> node2 [label=" a "];
    node1 -> node3 [label=" b "];
}

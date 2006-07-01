# basic.t

use Test::Base;
use FindBin;
use re::NFA;

my $Debug = 0;

plan tests => 2 * blocks();

run {
    my $block = shift;
    my $regex = $block->regex;
    my $graph = re::NFA->transform($regex);
    ok $graph, "$regex - re::Graph obj ok";
    my $gv = $graph->as_graphviz;
    my $got = $gv->as_debug;
    my $expected = $block->dot;
    $got =~ s/^\s+/    /gm;
    $got =~ s/label=" (\x{3b5}) "/label=" eps "/g;
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
    node3 [label="3"];
    node4 [label="4", shape="doublecircle"];
    node5 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node5 -> node1;
    node1 -> node2 [label=" a "];
    node2 -> node3 [label=" eps "];
    node3 -> node4 [label=" b "];
}



=== TEST 3:
--- regex: a*
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="1"];
    node2 [label="2"];
    node3 [label="3"];
    node4 [label="4", shape="doublecircle"];
    node5 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node5 -> node3;
    node1 -> node2 [label=" a "];
    node2 -> node1 [label=" eps "];
    node2 -> node4 [label=" eps "];
    node3 -> node1 [label=" eps "];
    node3 -> node4 [label=" eps "];
}



=== TEST 4:
--- regex: a|b
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="4"];
    node2 [label="6", shape="doublecircle"];
    node3 [label="1"];
    node4 [label="2"];
    node5 [label="3"];
    node6 [label="5"];
    node7 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node7 -> node6;
    node3 -> node4 [label=" a "];
    node4 -> node2 [label=" eps "];
    node5 -> node1 [label=" b "];
    node1 -> node2 [label=" eps "];
    node6 -> node3 [label=" eps "];
    node6 -> node5 [label=" eps "];
}



=== TEST 5:
--- regex: (a|b)*(aa|bb)(a|b)*
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="11"];
    node2 [label="12"];
    node3 [label="21"];
    node4 [label="22"];
    node5 [label="7"];
    node6 [label="5"];
    node7 [label="8"];
    node8 [label="2"];
    node9 [label="6"];
    node10 [label="17"];
    node11 [label="9"];
    node12 [label="13"];
    node13 [label="24"];
    node14 [label="1"];
    node15 [label="18"];
    node16 [label="25"];
    node17 [label="23"];
    node18 [label="19"];
    node19 [label="14"];
    node20 [label="16"];
    node21 [label="26", shape="doublecircle"];
    node22 [label="3"];
    node23 [label="4"];
    node24 [label="10"];
    node25 [label="20"];
    node26 [label="15"];
    node27 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node27 -> node5;
    node14 -> node8 [label=" a "];
    node24 -> node1 [label=" eps "];
    node1 -> node2 [label=" a "];
    node2 -> node15 [label=" eps "];
    node12 -> node19 [label=" b "];
    node19 -> node26 [label=" eps "];
    node26 -> node20 [label=" b "];
    node20 -> node15 [label=" eps "];
    node10 -> node12 [label=" eps "];
    node10 -> node11 [label=" eps "];
    node15 -> node16 [label=" eps "];
    node18 -> node25 [label=" a "];
    node8 -> node9 [label=" eps "];
    node25 -> node13 [label=" eps "];
    node3 -> node4 [label=" b "];
    node4 -> node13 [label=" eps "];
    node17 -> node18 [label=" eps "];
    node17 -> node3 [label=" eps "];
    node13 -> node17 [label=" eps "];
    node13 -> node21 [label=" eps "];
    node16 -> node17 [label=" eps "];
    node16 -> node21 [label=" eps "];
    node22 -> node23 [label=" b "];
    node23 -> node9 [label=" eps "];
    node6 -> node14 [label=" eps "];
    node6 -> node22 [label=" eps "];
    node9 -> node6 [label=" eps "];
    node9 -> node7 [label=" eps "];
    node5 -> node6 [label=" eps "];
    node5 -> node7 [label=" eps "];
    node7 -> node10 [label=" eps "];
    node11 -> node24 [label=" a "];
}



=== TEST 6:
--- regex: (a|)b*
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="6"];
    node2 [label="9"];
    node3 [label="3"];
    node4 [label="4"];
    node5 [label="7"];
    node6 [label="8"];
    node7 [label="10", shape="doublecircle"];
    node8 [label="2"];
    node9 [label="1"];
    node10 [label="5"];
    node11 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node11 -> node10;
    node9 -> node8 [label=" a "];
    node8 -> node1 [label=" eps "];
    node3 -> node4 [label=" eps "];
    node4 -> node1 [label=" eps "];
    node10 -> node9 [label=" eps "];
    node10 -> node3 [label=" eps "];
    node1 -> node2 [label=" eps "];
    node5 -> node6 [label=" b "];
    node6 -> node7 [label=" eps "];
    node6 -> node5 [label=" eps "];
    node2 -> node7 [label=" eps "];
    node2 -> node5 [label=" eps "];
}



=== TEST 7:
--- regex:
--- dot
digraph test {
    ratio="fill";
    node [fillcolor="yellow", shape="circle", style="filled"];
    edge [color="red"];
    node1 [label="1"];
    node2 [label="2", shape="doublecircle"];
    node3 [fillcolor="white", height="0", label=" ", shape="plaintext", width="0"];
    node3 -> node1;
    node1 -> node2 [label=" eps "];
}

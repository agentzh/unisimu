use GraphViz;

my $g = GraphViz->new('no_overlap' => 1,
					   'ratio' => 'auto',
					  
					  #'layout' => 'neato'
					  );
my $a = 10;
my $b = [];
$g->add_node($a);
$g->add_node($b, label => $a);
$g->add_edge($a => $b);

$g->as_png('t1.png');
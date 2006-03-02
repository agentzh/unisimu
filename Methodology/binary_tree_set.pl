use strict;
use warnings;
use GraphViz;
use Data::Dumper;
use List::MoreUtils 'all';

my $MAX_LEVELS = shift || 3;

my $gv = GraphViz->new(
    node => {shape => 'circle', style => 'filled', fillcolor => 'yellow'},
    edge => {color => 'red'},
);
my $nnodes = 0;

my %tree;
sub gen_tree {
    my ($x, $level) = @_;
    $level ||= 0;
    return $x if $level >= $MAX_LEVELS;
    $tree{$x} = [
        gen_tree(2*$x+1, $level+1),
        gen_tree(3*$x+1, $level+1),
    ];
    return $x;
}

gen_tree(1);

sub plot_tree {
    my $root = shift;
    return if not defined $root;
    $gv->add_node($root);
    return if not $tree{$root};
    my ($left, $right) = @{ $tree{$root} };
    $gv->add_edge($root, $left, label => 'L') if defined $left;
    $gv->add_edge($root, $right, label => 'R') if defined $right;
    plot_tree($left);
    plot_tree($right);
}

plot_tree(1);
$gv->as_png('set.png');

my %visited;
sub adjust_tree {
    my ($root, $level) = @_;
    $level ||= 0;
    return if not $tree{$root};
    my ($left, $right) = @{ $tree{$root} };
    adjust_child($root, $left, $level+1);
    adjust_child($root, $right, $level+1);
}

sub adjust_child {
    my ($parent, $child, $level) = @_;
    return if not defined $child;
    my $info = $visited{$child};
    if ($info) {
        my ($former_parent, $former_level) = @$info;
        if ($former_level > $level) {
            del_edge($former_parent, $child);
        } else {
            del_edge($parent, $child);
        }
    } else {
        $visited{$child} = [$parent, $level];
        adjust_tree($child, $level);
    }
}

#adjust_tree(1);
$gv = GraphViz->new(
    node => {shape => 'circle', style => 'filled', fillcolor => 'yellow'},
    edge => {color => 'red'},
);
#plot_tree(1);
#$gv->as_png('set2.png');

sub del_edge {
    my ($parent, $child) = @_;
    warn "Deleting edge $parent <=> $child...";
    my ($left, $right) = @{ $tree{$parent} };
    if ($left eq $child) {
        $tree{$parent} = [undef, $right];
    } else {
        $tree{$parent} = [$left, undef];
    }
}

my @levels;
sub refactor {
    my ($root, $level) = @_;
    return if not defined $root;
    $level ||= 0;
    $levels[$level] ||= [];
    push @{ $levels[$level] }, $root;
    return if not $tree{$root};
    my ($left, $right) = @{ $tree{$root} };
    refactor($left,  $level+1);
    refactor($right, $level+1);
}

refactor(1);
#die Data::Dumper->Dump([\@levels], [qw(levels)]);

print join("\n", map { join(' ', @$_) } @levels), "\n";

foreach my $i (1..$#levels) {
    die "Assumption failed for level $i and " . ($i-1)
        if not check($i);
}
print "Assumption holds for levels 0..$#levels\n";

sub check {
    my $i = shift;
    my @array_a = @{ $levels[$i-1] };
    my @array_b = @{ $levels[$i] };
    all {
        my $elem = $_;
        all { $elem >= $_ } @array_a;
    } @array_b;
}

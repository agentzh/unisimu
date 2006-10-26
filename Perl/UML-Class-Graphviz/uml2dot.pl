use strict;
use warnings;

use Getopt::Std;
use UML_Class_Graphviz;
#use Data::Dump::Streamer;

my %opts;
getopts('h', \%opts);
if ($opts{h}) {
    die "Usage: uml2dot fast.uml";
}

my $c = 0;
my @classes;
my $class;
while (<>) {
    chomp;
    s/^\s+|\s+$//g;
    if (!$_) { die "syntax error: line $.: empty line is unexpected here.\n"; }
    if ($c == 0) {
        $class = { name => $_ };
    } elsif ($c == 1) {
        $class->{methods} = [split /\s+/, $_] if $_ ne '-';
    } elsif ($c == 2) {
        $class->{properties} = [split /\s+/, $_] if $_ ne '-';
    } elsif ($c == 3) {
        if ($_ ne '-') {
            $class->{subclasses} = [split /\s+/, $_];
        }
        push @classes, $class;
        my $sep = <>;
        last if !defined $sep;
        chomp $sep;
        if ($sep) {
            die "syntax error: line $.: empty line is expected here.\n";
        }
        $c = 0;
        next;
    }
    $c++;
}
my $data = { classes => \@classes };
#warn Dump($data)->Out();
print UML::Class::GraphViz->as_dot($data);

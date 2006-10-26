use strict;
use warnings;
my $name = shift;
map { s/"/\\"/g; $_ = "\"$_\"" } @ARGV;

if (system("$^X rt2uml.pl @ARGV > $name.uml") == 0) {
    warn "$name.uml generated.\n";
} else {
    die "can't generate $name.uml.\n";
}
if (system("$^X uml2dot.pl $name.uml > $name.dot") == 0) {
    warn "$name.dot generated.\n";
} else {
    die "can't generate $name.dot.\n";
}
if (system("dot -T png $name.dot > $name.png") == 0) {
    warn "$name.png generated.\n";
} else {
    die "can't generate $name.png.\n";
}

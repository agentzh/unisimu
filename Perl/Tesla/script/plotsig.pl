#: plotsig.pl
#: Plot signal timing diagrams using Maple commands.
#: Tesla v0.04
#: Agent2002. All rights reserved.
#: 04-11-14 04-12-14

use strict;
use warnings;

my $numpat = qr/(?:\.\d+|\d+(?:\.\d*)?)/;
my $eventpat = qr/(?:[01UXZ]\s*\@\s*$numpat)/;
my $eventlistpat = qr/(?:$eventpat(?:\s*,\s*$eventpat)*)/;

my @siglist;
my $title = '';

my $state = 1;
while (<>) {
    # warn "state = $state";
    if ($state == 1) {
        if (!match($_)) {
            $title = $_;
            chomp($title);
            next;
        }
        push @siglist, $_;
        $state = 2;
        next;
    }
    if (match($_)) {
        push @siglist, $_;
        next;
    }
    local $" = "# ";
    output( "# $title\n# @siglist\n" );
    plot_sig_list( $title, @siglist );

    @siglist = ();
    $title = $_;
    chomp($title);
    $state = 1;
}

if ($state == 2) {
    local $" = "# ";
    output( "# $title\n# @siglist\n" );
    plot_sig_list( $title, @siglist );
} 

sub plot_sig_list {
    # warn "HERE!";
    my $title = shift;
    my @siglist = reverse @_;
    my $max_time = 0;
    map { m/\@($numpat)\s*$/;
          $max_time = $1 if $1 > $max_time; } @siglist;
    $max_time += $max_time / 10;
    my @plots;
    for (my $i = 0; $i < @siglist; $i++) {
        push @plots, plot_sig( $max_time, $i, $siglist[$i] );
    }
    local $" = ',';
    output( qq/plots[display]({@plots},title="$title");\n\n/ );
    output( 'print(`\n\n`);' );
}

sub plot_sig {
    my $max_time = shift;
    my $index = shift;
    $_[0] =~ m/(\w+)\s*:\s*($eventlistpat)/;
    my $signame = $1;
    my @events = split( /\s*,\s*/, $2 );
    map { m/(.+)\@(.+)/; $_ = [$2,$1]; } @events;
    
    my $endpoint = [ $max_time, $events[-1]->[1] ];
    my @points = ( shift(@events) );
    foreach my $point (@events) {
        my $prev = [ $point->[0], !$point->[1]?1:0 ];
        push @points, $prev, $point;
    }
    push @points, $endpoint;

    map { my ($x, $y) = @$_;
          $y = adjust($y) + $index;
          $_ = "[$x,$y]"; } @points;

    my $plotobj = "SIG_$signame";
    local $" = ',';
    my $height = 0.5 + $index;
    output( <<"_EOC_" );
plots[display](CURVES([@points]),thickness=2,color=blue):
plots[textplot]([0.1,$height,"$signame"],align={RIGHT}):
$plotobj:=plots[display]([%,%%],axes=boxed,labels=["t/ns",""]):

_EOC_
    return $plotobj;
}

sub match {
    return $_[0] =~ m/^\s* \w+ \s*:\s* $eventlistpat \s*$/x;
}

sub adjust {
    if ($_[0]) { return 0.9; }
    return 0.1;
}

sub output {
    print @_;
}

0;


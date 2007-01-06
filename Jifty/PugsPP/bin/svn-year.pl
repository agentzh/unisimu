use strict;
use warnings;

use lib 'lib';
use List::Util qw(min max);
use GD::Graph::lines;
use Jifty::Everything;
use Cache::FileCache;
use Getopt::Std;
use YAML::Syck;

my %opts;
getopts('fu', \%opts);

my $cache = new Cache::FileCache(
    { 'namespace' => 'svn',
      'default_expires_in' => 3600,
    }
);

my $debug = 0;

BEGIN {
    Jifty->new;
}

use PugsPP::Model::SvnLog;

my $year = shift @ARGV || '2005';
my @senders = @ARGV;
my $key = "$year-" . (join('-', @senders) || 'all');
my $points = $cache->get($key);
if (!$points or $opts{f}) {
    $points = gen_data_for_year($year);
    $cache->set($key => $points);
}
#warn "@$points\n";
#warn "len = ", scalar(@$points), "\n";
#warn Dump($points);

my @data = ( 
    [ qw( Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec ) ],
    [ @$points ],
);

my $my_graph = new GD::Graph::lines();

$my_graph->set( 
	x_label => 'Month',
	y_label => 'Count',
	title => "Pugs New Commits for $year (" . ("@senders" || 'all') . ")",

	y_max_value => $opts{u} ? undef : 2000,
	y_min_value => 0,
	y_tick_number => 4,

	y_label_skip => 1,
	box_axis => 1,
	line_width => 3,

	transparent => 0,
);

$my_graph->plot(\@data);
save_chart($my_graph, "irc-$year");

sub gen_data_for_year {
    my $year = shift;
    my $msg_model = PugsPP::Model::SvnLogCollection->new;
    my @points;
    for my $month (1..12) {
        $month = "0$month" if length($month) == 1;
        for my $sender (@senders) {
            $msg_model->limit(
                column => 'committer', operator => 'like', value => $sender,
                entry_aggregator => 'OR',
            );
        }
        $msg_model->limit(
            column => 'committed', operator => '>=', value => "$year-$month-01 00:00",
            entry_aggregator => 'AND'
        );
        my $next_month = $month + 1;
        my $value;
        if ($next_month > 12) {
            $year++;
            $next_month = '01';
        }
        $next_month = "0$next_month" if length($next_month) == 1;
        $msg_model->limit(
            column => 'committed', operator => '<', value => "$year-$next_month-01 00:00",
            entry_aggregator => 'AND'
        );
        my $nmsgs = $msg_model->count;
        warn "$year-$month: $nmsgs commits\n";
        $msg_model->unlimit;
        push @points, $nmsgs;
    }
    $year--;
    \@points;
}

# The reverse is in here, because I thought the falling line was 
# depressing, but I was too lazy to retype the data set

#my @points = reverse(4, 3, 5, 6, 3,  1.5, -1, -3, -4);
#my $min_y = min(@points) - 500;
#$min_y = 0 if $min_y < 0;
#my $max_y = max(@points) + 500;

sub save_chart
{
    my ($chart, $name) = @_;
	my $ext = $chart->export_format;

    my $fname = "svn-$key.$ext";
	open(my $out, "> $fname") or 
		die "Cannot open $name.$ext for write: $!";
	binmode $out;
	print $out $chart->gd->$ext();
	close $out;
    warn "$fname generated.\n";
}

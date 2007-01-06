use strict;
use warnings;

use DateTime;
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
    { 'namespace' => 'irc',
      'default_expires_in' => 3600 * 24 * 7,
    }
);

my $debug = 0;

BEGIN {
    Jifty->new;
}

use PugsPP::Model::IRCMessage;

my $year = shift @ARGV || '2005';
my $month = shift @ARGV || '03';
my @senders = @ARGV;
my $key = "$year-$month-" . (join('-', @senders) || 'all');

my $last_day = DateTime->last_day_of_month( year => $year, month => $month )->day;
warn "Last day of month: $last_day\n";

my $points = $cache->get($key);
if (!$points or $opts{f}) {
    $points = gen_data_for_month($year => $month => $last_day);
    $cache->set($key => $points);
}
#warn "@$points\n";
#warn "len = ", scalar(@$points), "\n";
#warn Dump($points);


my @data = (
    [ 1..$last_day ],
    [ @$points ],
);

my $my_graph = new GD::Graph::lines();

$my_graph->set( 
	x_label => 'Month',
	y_label => 'Count',
	title => "#perl6 Messages for $year-$month (" . ("@senders" || 'all') . ")",

	y_max_value => $opts{u} ? undef : 4000,
	y_min_value => 0,
	y_tick_number => 5,

	y_label_skip => 2,
	x_label_skip => 3,
    box_axis => 1,
	line_width => 3,

	transparent => 0,
);

$my_graph->plot(\@data);
save_chart($my_graph, "irc-$year");

sub gen_data_for_month {
    my ($year, $month, $last_day) = @_;
    my $msg_model = PugsPP::Model::IRCMessageCollection->new;
    my @points;
    my $dt_today = DateTime->new(year => $year, month => $month, day => 1);
    #warn "init: $dt_today\n";
    #warn "init month: ", $dt_today->month, "\n";
    while ($dt_today->month == $month) {
        for my $sender (@senders) {
            $msg_model->limit(
                column => 'sender', operator => 'like', value => $sender,
                entry_aggregator => 'OR',
            );
        }
        $msg_model->limit(
            column => 'sent', operator => '>=', value => $dt_today->ymd . " 00:00",
            entry_aggregator => 'AND'
        );
        my $day = $dt_today->day;
        $dt_today->add( days => 1 );
        $msg_model->limit(
            column => 'sent', operator => '<', value => $dt_today->ymd . " 00:00",
            entry_aggregator => 'AND'
        );
        my $nmsgs = $msg_model->count;
        warn "$year-$month-$day: $nmsgs msg\n";
        $msg_model->unlimit;
        push @points, $nmsgs;
    }
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

    my $fname = "irc-$key.$ext";
	open(my $out, "> $fname") or 
		die "Cannot open $name.$ext for write: $!";
	binmode $out;
	print $out $chart->gd->$ext();
	close $out;
    warn "$fname generated.\n";
}

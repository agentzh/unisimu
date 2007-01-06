use strict;
use warnings;

use lib 'lib';
use Jifty::Everything;

BEGIN { Jifty->new; }

use DateTime;
use PugsPP::Model::SvnLog;

my $log_model = PugsPP::Model::SvnLog->new;

while (<>) {
    next if /^-----------+$/;
    if (/^r\d+ \(orig r(\d+)\):  (\S+) \| (\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}) ([-+]\d{4})/) {
        my ($rev, $committer, $committed, $time_zone) = ($1, $2, $3, $4);
        #print "rev: $rev\ncommitter: $committer\ncommitted: $committed\ntime_zone: $time_zone\n";
        my $dt = DateTime->new(parse_iso($committed), time_zone => $time_zone);
        $dt->set_time_zone('GMT');
        #print "New timestamp: ", $dt->ymd, " ", $dt->hms, "\n";
        #print "--------------------------------\n";
        print "r$rev\n" if $rev % 500 == 0;
        $log_model->load_or_create(
            revision => $rev,
            committer => $committer,
            committed => $committed,
        );
    }
    elsif (/orig r\d+/) {
        die "syntax error: $_";
    }
}

sub parse_iso {
    my @item = split /\s+|:|-/, shift;
    return (
        year => $item[0],
        month => $item[1],
        day => $item[2],
        hour => $item[3],
        minute => $item[4],
        second => $item[5],
    );
}

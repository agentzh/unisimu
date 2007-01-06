use strict;
use warnings;

use lib 'lib';
use YAML::Syck;
use Getopt::Std;

use Jifty::Everything;
BEGIN { Jifty->new }

use PugsPP::Model::IRCSession;
use PugsPP::Model::IRCMessage;

my %opts;
getopts('f', \%opts);

my $session_model = PugsPP::Model::IRCSession->new;
my $message_model = PugsPP::Model::IRCMessage->new;

$YAML::Syck::ImplicitUnicode = 1;

my @files = sort map glob, @ARGV;
for my $file (@files) {
    if ($file =~ /\.yml/) {
        my $session_count = import_file($file);
        warn "$file imported ($session_count sessions created).\n";
    } else {
        warn "Warning: $file: not ending with .yml. skipped.\n";
    }
}

sub import_file {
    my $fname = shift;
    my $sessions = LoadFile($fname);
    my $session_count = 0;
    for my $session (@$sessions) {
        next if !@$session;
        my $begin_time = $session->[0]->{sent};
        my $end_time = $session->[-1]->{sent};
        $session_model->load_by_cols(
            begin_time => $begin_time,
            end_time   => $end_time,
            msg_count  => scalar(@$session),
        );
        next if !$opts{f} and defined $session_model->id;
        my ($session_id, $error) = $session_model->create(
            begin_time => $begin_time,
            end_time   => $end_time,
            msg_count  => scalar(@$session),
        );
        if (!$session_id) {
            die "Failed to create session ($begin_time ~ $end_time):\n\t$error";
        }
        $session_count++;
        my $offset;
        for my $msg (@$session) {
            my ($id) = $message_model->create(
                %$msg,
                irc_session => $session_id,
                session_offset => $offset++,
            );
            if (!$id) {
                die "Failed to create message ($msg->{sender} at $msg->{sent}):\n\t$error";
            }
        }
    }
    return $session_count;
}

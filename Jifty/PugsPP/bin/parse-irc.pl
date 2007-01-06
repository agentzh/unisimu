use strict;
use warnings;

use Time::Simple;
use YAML::Syck;
use Getopt::Std;
use Encode 'encode';
use Encode::Guess;

my %opts;
getopts('hf', \%opts) or help();

my @files = sort map glob, @ARGV;
@files || help();

binmode(\*STDOUT, ":encoding(UTF-8)");

$YAML::Syck::ImplicitUnicode = 1;

for my $file (@files) {
    if ($file =~ /\d{4}-\d{2}-\d{2}/) {
        my $outfile = "$file.yml";
        if (!$opts{f} and -f $outfile and -s $outfile) {
            warn "$outfile already exists. skipped.\n";
            next;
        }
        my ($sessions, $msg_count) = parse_file($&, $file);
        DumpFile($outfile, $sessions);
        my $nsessions = scalar(@$sessions);
        warn "  $outfile generated ($msg_count msg, $nsessions session).\n";
    } else {
        die "$file: no date found in the file name. skipped.\n";
    }
}

sub parse_file {
    my ($date, $fname) = @_;
    open (my $in, $fname) or
       die "Can't open $fname for reading: $!";
    my ($prev_time, @session, $msg_count);
    while (<$in>) {
        next if /^\s*$/;
        chomp;
        my ($hms, $sent, $sender, $content, $type);
        if (/^\[([\d:]+)\]/g) {
            $hms = $1;
            $sent = "$date $hms";
        } else {
            die "$fname: line $.: syntax error: $_\n";
        }
        if (/\G <([^>]+)> (.*)/gc) {
            ($sender, $content) = ($1, $2);
            $type = 'N';
        }
        elsif (/\G \* (\S+) (.*)/gc) {
            ($sender, $content) = ($1, "$1 $2");
            $type = 'A';
        }
        else {
            # ignore system messages for now
            next;
        }
        my @enc;
        if ($content =~ /^([[:print:]]*[A-Za-z]+[^[:print:]]{1,5}[A-Za-z]+[[:print:]]*)+$/ or
            $content =~ /^[[:print:]]*[^[:print:]]{1,5}[A-Za-z]+[[:print:]]*$/ ) {
            @enc = qw(latin1 fr euc-cn big5-eten);
        } else {
            @enc = qw(euc-cn big5 latin1 fr);
        }
        my $utf8 = decode_by_guessing(
            $content,
            qw/ascii utf-8/, @enc,
        );
        if (!$utf8) {
            warn "Warning: $fname(line $.): malformed data: ", Dump($content);
            $content =~ s/[^[:print:]]+/?/gs;
            warn "\tadjusted to \"$content\"\n";
        } else {
            $content = encode('UTF-8', $utf8);
        }
        my $msg = {
            sent => $sent,
            sender => $sender,
            msg_type => $type,
            content => $content,
        };
        $msg_count++;
        my $current_time = Time::Simple->new($hms);
        if ($prev_time) {
            if ($prev_time - $current_time > 20 * 60) {
                push @session, [$msg];
            } else {
                push @{ $session[-1] }, $msg;
            }
        } else {
            push @session, [$msg];
        }
        $prev_time = $current_time;
    }
    close $in;
    return (\@session, $msg_count);
}

sub decode_by_guessing {
    my $s = shift;
    my @enc = @_;
    for my $enc (@enc) {
        my $decoder = guess_encoding($s, $enc);
        if (ref $decoder) {
            if ($enc ne 'ascii') {
                print "line $.: $enc message found: ", $decoder->decode($s), "\n";
            }
            return $decoder->decode($s);
        }
    }
    undef;
}

sub help {
    print <<_EOC_;
Usage: $0 <irc-log-file> ...
Options:
    -h    Print this help.
    -f    Force regen .yml files even if they exist
_EOC_
    exit(0);
}

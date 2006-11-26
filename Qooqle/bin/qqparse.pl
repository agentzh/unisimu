# qqparse.pl - parse qq log files to .data files

use strict;
use warnings;

use File::Slurp;
use File::Spec;
use YAML::Syck;
use DateTime;
#use Data::Dumper;
use Encode qw(decode);
use lib 'lib';
use Qooqle::Message::Splitter;

#$Data::Dumper::Indent = 1;
#$YAML::Syck::ImplicitUnicode = 1;

my $Debug = 0;

my $HeaderPattern = <<'_EOC_';
用户(?::|：)[^\n]*

===========+
\S+
===========+
\S+
===========+

--------------------------------------------------
消息对象(?::|：)\s*([^\n]*)
--------------------------------------------------
_EOC_

my @infiles = map glob, @ARGV or
    die "error: no input file given.\n";

for my $infile (@infiles) {
    parse_log($infile);
}

sub parse_log {
    my $logfile = shift;
    my $outfile = $logfile . '.data';
    my $path = File::Spec->rel2abs($logfile);
    my @dirs = File::Spec->splitdir($path);
    my $s;
    if (@dirs == 1) {
        $s = $logfile;
    } else {
        $s = join('/', $dirs[-2], $dirs[-1]);
    }
    #print "info: $s\n";
    open my $in, $logfile or
        die "Can't open $logfile for reading: $!\n";
    #sleep(1);
    my $realname = $dirs[-2] if @dirs >= 2;
    $realname = ($realname =~ /^[^\w]+$/) ? $realname : undef;
    #warn "    info: $realname\n" if $realname;

    my $txt = read_file($logfile);
    my ($friend_info, $content);
    if ($txt =~ /$HeaderPattern/os) {
        ($friend_info, $content) = ($1, $');
        #warn "[0] Matched: $s ($friend_info)\n" if $Debug;
    } else {
        die "QQ log file with unknown formats: $logfile\n";
    }
    my ($friend_name, $friend_id);
    if ($friend_info =~ /(?x) ^ \s* (.+?) \s* (?: \( |（) (\d{4,}) (?: \) |）) \s* $/) {
        ($friend_name, $friend_id) = ($1, $2);
    }
    elsif ($friend_info =~ /(?x) ^ \s* (\d{4,}) \s* (?: \( |（) (.+?) (?: \) |）) \s* $/) {
        ($friend_name, $friend_id) = ($2, $1);
    }
    else {
        die "QQ log file with unknown friend info: $friend_info\n";
    }
    warn "[1] Matched: $s ($friend_id => $friend_name)\n" if $Debug;
    my $sessions = parse_content("\n$content", $friend_name);
    my $data = {
        type        => 'personal',
        realname    => to_utf8($realname),
        nickname    => to_utf8($friend_name),
        qq_number   => $friend_id,
        sessions    => $sessions,
    };
    print "  info: generating $outfile...\n";
    DumpFile($outfile, $data);
}

sub parse_content {
    my ($src, $friend_name) = @_;
    open my $fh, '<', \$src or die;
    my ($ready, @msg, @sessions, $msg_count);
    my $splitter = Qooqle::Message::Splitter->new(20 * 60); # 20 min
    while (<$fh>) {
        #warn $.;
        if (!$ready and /^$/) { $ready = 1; }
        elsif ($ready and /(?x) (\d{4})-(\d{2})-(\d{2}) \s+ (\d{2}):(\d{2}):(\d{2}) \s+ (.*)/) {
            my @bits = ($1, $2, $3, $4, $5, $6);
            my $sender = $7;
            my $datetime = new_datetime(@bits);
            my $timestamp = $datetime->ymd . ' ' . $datetime->hms;
            my $category = $sender eq $friend_name ? 'a' : 'b';
            $splitter->add($category => $datetime->epoch);
            if ($splitter->should_split) {
                if (@msg) {
                    $msg[-1][-1] =~ s/(?:[\s\n]|　)+\Z//gs;
                    push @sessions, [map { to_utf8($_) } @msg];
                    @msg = ();
                }
            }
            #die "Matched! $timestamp";
            if (@msg) {
                $msg[-1][-1] =~ s/(?:[\s\n]|　)+\Z//gs;
            }
            push @msg, [$timestamp, $sender, ''];
            $msg_count++;
        } else {
            undef $ready if $ready;
            $msg[-1][-1] .= $_ if @msg;
        }
    }
    close $fh;
    if (@msg) {
        $msg[-1][-1] =~ s/(?:[\s\n]|　)+\Z//gs if @msg;
        push @sessions, [map { to_utf8($_) } @msg];
    }
    print "  info: ", scalar(@sessions),
        " sessions and $msg_count messages found.\n";
    \@sessions;
}

sub new_datetime {
    my ($year, $mon, $day, $hour, $min, $sec) = @_;
    DateTime->new(
        year   => $year,
        month  => $mon,
        day    => $day,
        hour   => $hour,
        minute => $min,
        second => $sec,
        time_zone => 'Asia/Shanghai',
    );
}

sub to_utf8 {
    my $data = shift;
    if (ref $data) {
        map { $_ = to_utf8($_) } @$data;
    } else {
        $data = decode("gbk", $data);
    }
    $data;
}

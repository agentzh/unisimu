# qqimport.pl - import .data files to the Qooqle database

use strict;
use warnings;
use lib 'lib';

use YAML::Syck;
use Encode 'decode';
use UNIVERSAL::require;
#use Data::Dumper;
use File::Slurp;
use Jifty::Config;
use Jifty::ClassLoader;

BEGIN {
    Jifty::ClassLoader->new(base => 'Qooqle')->require;
    binmode(\*STDERR, ":encoding(GBK)");
    binmode(\*STDOUT, ":encoding(GBK)");
}

$YAML::Syck::ImplicitUnicode = 1;

use Qooqle::Model::QQUser;
use Qooqle::Model::Session;
use Qooqle::Model::Message;

my @infiles = map glob, @ARGV or
    die "error: no input file given.\n";

my $config = Jifty::Config->new;
$config->load;
my $db_config = $config->framework('Database');
my $qq_config = $config->framework('QQ');
#warn Dumper($qq_config);
my $my_qq   = $qq_config->{my_qq_number};
my $my_nick = decode('GBK', $qq_config->{my_qq_nickname});
my $my_real = decode('GBK', $qq_config->{my_qq_realname});

use Jifty::DBI::Handle;
my $handle = Jifty::DBI::Handle->new();
$handle->connect(
    driver   => $db_config->{Driver},
    database => $db_config->{Database},
    user     => $db_config->{User},
    password => $db_config->{Password},
);
#$handle->connect;
Jifty->new( handle => $handle );

my $User    = Qooqle::Model::QQUser->new;
my $Session = Qooqle::Model::Session->new;
my $Message = Qooqle::Model::Message->new;

register_user($my_qq, $my_nick, $my_real) if $my_qq;

for my $infile (@infiles) {
    if ($infile !~ /\.data$/i) {
        warn "warning: $infile is not a .data file. skipped.\n";
        next;
    }
    process_file($infile);
}

sub process_file {
    my $infile = shift;
    my $data;
    $data = LoadFile($infile);
    #write_file('tmp.txt', Dumper($data));
    if ($@) {
        die "error: failed to load $infile: $@";
    }
    if ($data->{type} eq 'personal') {
        my $friend_nick = $data->{nickname};
        my $friend_real = $data->{realname};
        my $friend_qq   = $data->{qq_number};
        register_user($friend_qq, $friend_nick, $friend_real);
        print <<"_EOC_";
---
nickname: $friend_nick
real name: $friend_real
qq number: $friend_qq
_EOC_
        my $sessions = $data->{sessions};
        if (!$sessions) {
            warn "warning: no sessions found.\n";
            return;
        }
        for my $session (@$sessions) {
            next if !$session;
            my $msg_count = @$session;
            my $begin_time = $session->[0]->[0];
            my $end_time = $session->[-1]->[0];
            print "session found: $begin_time ~ $end_time ($msg_count msg).\n";

            #warn "HERE!";
            #$msg_count
            $Session->load_by_cols(
                begin_time    => $begin_time,
                end_time      => $end_time,
                message_count => $msg_count,
            );
            if (defined $Session->id) {
                #warn "session already exists ($infile - $begin_time)";
                next;
            }
            $Session->create(
                begin_time    => $begin_time,
                end_time      => $end_time,
                message_count => $msg_count,
            );
            if (!defined $Session->id) {
                die "Failed to create session ($infile - $begin_time)";
            }
            my $offset = 0;
            for my $msg (@$session) {
                my ($sender, $receiver);
                if ($msg->[1] eq $friend_nick) {
                    $sender   = $friend_qq;
                    $receiver = $my_qq;
                } elsif ($msg->[1] eq $my_nick) {
                    $sender   = $my_qq;
                    $receiver = $friend_qq;
                } else {
                    $sender   = nick_to_number($msg->[1]);
                    $receiver = $friend_qq;
                }
                #warn "$msg->[1], sender: $sender -- receiver: $receiver\n";
                $Message->create(
                    sender   => $sender,
                    receiver => $receiver,
                    content  => $msg->[2],
                    sent     => $msg->[0],
                    msg_session     => $Session->id, 
                    session_offset => $offset++,
                );
                if (!defined $Message->id) {
                    die "Failed to create message";
                }
                #warn "message created.\n";
            }
            print "session created.\n";
        }
    } else {
        warn $data->{type}, " type is not supported.\n";
    }
}

sub register_user {
    my ($qq_number, $nickname, $realname) = @_;
    $User->load_by_cols( qq_number => $qq_number );
    if (defined $User->id) {
        if ($realname) {
            $User->set_realname( $realname );
        }
        if ($nickname) {
            $User->set_nickname( $nickname );
        }
    } else {
        $User->create(
            qq_number => $qq_number,
            realname  => $realname,
            nickname  => $nickname,
        );
        if (!defined $User->id) {
            die "error: failed to create user $qq_number";
        } else {
            print "user $qq_number ($nickname) created.\n";
        }
    }
}

sub nick_to_number {
    my ($nickname) = @_;
    $User->load_by_cols( nickname => $nickname );
    if (defined $User->id) {
        return $User->qq_number;
    } else {
        $User->create(
            qq_number => '888888',  # place-holder
            nickname  => $nickname,
        );
        if (!defined $User->id) {
            die "error: failed to create user $nickname";
        } else {
            print "user $nickname created.\n";
        }
        return '888888';
    }
}

#: reminder.pl
#: 2006-07-12 2006-07-21

use strict;
use warnings;

use Win32;
use DateTime;
use File::Log;
use FindBin;
use Win32::OLE;
use Data::Dumper;
use Win32::Process qw[ STILL_ACTIVE NORMAL_PRIORITY_CLASS ];

my $sleep_cycle = 5 * 60 * 1000; # in miniseconds
my $time_out = 30;               # in seconds
my $force_close = 1;
my $reboot = 0;
my $me = $ENV{COMPUTERNAME};
my $log_file = "$FindBin::Bin/reminder.log";
my $log = File::Log->new(
    logFileName => $log_file,
    dateTimeStamp => 0,
    stderrRedirect => 0,
);

my $username = Win32::LoginName;
my $start = now();
my $now = $start;

my @off_plan = (
    ['11:30' => '12:30' => "Good morning, $username, it is lunch time already!"],
    ['17:00' => '18:30' =>
        "Good afternoon, $username! I think it is time for supper. See you later!"],
    ['21:00' => '23:59' =>
        "Good evening, $username! I think it is time to sleep. ".
        "I'll go off in $time_out seconds."],
    ['00:00' => '05:00' =>  "Oh, I am still sleepy. Don't bother me, $username!"],
);

run() if ! caller;

sub run {
    logger("$0 starts at ".$start->hms);
    while (1) {
        #print "Hey!\n";
        logger("resumes and checks");
        $now = now();
        check_rest_time($now);
        my $hms = $now->hms;
        check_rest_time($now);
        check_off_plan($hms);
        #warn $hms;
        Win32::Sleep($sleep_cycle);
    }
    exit(0);
}

sub check_rest_time {
    my $now = shift;
    if ($now->hour - $start->hour >= 1) {
        remind("Hey, $username, I think you need a break!");
        $start = $now;
    }
}

sub check_off_plan {
    my $now = shift;
    for my $hook (@off_plan) {
        my ($from, $to, $msg) = @$hook;
        $from .= ':00';
        $to .= ':00';
        if ($now ge $from && $now le $to) {
            logger("try to shut down the machine at $now ($msg)");
            shut_down($msg);
        }
    }
}

sub remind {
    my $msg = shift;
    logger("remind me ($msg)");
    my $sp = Win32::OLE->new('SAPI.SpVoice');
    #warn $sp;
    if ($sp) {
        my $voices = $sp->GetVoices;
        my @voices;
        for my $i (0..$voices->Count-1) {
            push @voices, $voices->Item($i);
        }
        #warn "voices = @voices";
        warn "  info: Current Voice is ", $sp->voice->GetDescription, "\n";
        if ($sp->voice->GetDescription =~ /Chinese/i) {
            if (@voices == 1) {
                msgbox($msg);
                return;
            }
            for my $voice (@voices) {
                my $name = $voice->GetDescription;
                if ($name !~ /Chinese/i) {
                    warn "  info: Changing voice to $name...\n";
                    $sp->{voice} = $voice;
                    last;
                }
            }
        }
        #warn "HERE!";
        $sp->Speak($msg);
        msgbox($msg);
        #$sp->WaitUntilDone(2000);
    } else {
        msgbox($msg);
    }
}

sub msgbox {
    my $msg = shift;
    my $process;
    my $quoted = Dumper($msg);
    $quoted =~ s/\$[^=]+=//;
    $quoted =~ s/"/\\"/g;
    $quoted =~ s/;\n$//;
    my $cmd = qq[$^X -S msgbox.bat "$quoted"];
    warn "  info: cmd: $cmd\n";
    Win32::Process::Create($process,
                           "$^X",
                           $cmd,
                           0,
                           NORMAL_PRIORITY_CLASS,
                           ".");
    my $exitcode;
    $process->GetExitCode($exitcode);
    if ($exitcode != STILL_ACTIVE) {
        if ($exitcode == 1) {
            warn $msg;
            Win32::MsgBox($msg, MB_ICONINFORMATION, "${username}'s reminder");
        }
    } else {
        $process->Wait($time_out * 1000);
        warn "  info: Killing MsgBox...\n";
        $process->Kill(0);
    }
}

sub shut_down {
    my $message = shift;
    remind($message);
    Win32::InitiateSystemShutdown(
        $me, $message, $time_out,
        $force_close, $reboot
    );
}

sub logger {
    my $msg = join('', @_);
    my $time = $now->ymd . ' ' . $now->hms;
    $log->msg(1, "[$time] $msg\n");
}

sub now {
    my $dt = DateTime->now;
    $dt->set_time_zone( 'Asia/Shanghai' );
    $dt;
}

1;
__END__

Sub TryTTS()
    Dim sp As New SpVoice
    Dim voices As ISpeechObjectTokens
    Dim voice As SpObjectToken
    
    Set voices = sp.GetVoices
    MsgBox "Default Voice: " & sp.voice.GetDescription
    MsgBox voices.Count
    For i = 0 To voices.Count - 1
        Set voice = voices.Item(i)
        MsgBox "Voice of " & voice.GetDescription
        Set sp.voice = voice
        sp.Speak "Hello!", SVSFlagsAsync
        sp.WaitUntilDone (2000) 'Time out is 2 sec
    Next i
End Sub

#: david.pl
#: Copyright (c) Agent Zhang
#: 2004-12-05 2005-11-18

use strict;
use warnings;

use Win32::Mutex;
use Win32::Process;
use Win32;
use Cwd;
use Time::HiRes;

#die "usage: david <command-line>\n" unless @ARGV;

sub ErrorReport{
    print Win32::FormatMessage( Win32::GetLastError() );
}

my $name = 'MutexToDavidDebugger';
my $name2 = 'Mutex2ToDavidDebugger';

my $mutex = Win32::Mutex->new(1,$name);

#my $progname = shift;
#my $args = @ARGV? join('',@ARGV) : "";
#my $fullpath = cwd()."/$progname";
#warn "Starting $progname...\n";

#Win32::Process::Create( my $process,
#          $fullpath,
#          "$progname $args",
#          0,
#          NORMAL_PRIORITY_CLASS,
#          ".") || die ErrorReport();

$| = 1;
print "Please start your client executable now, I'm ready...";
#<STDIN>;

my $mutex2;

while ( !($mutex2 = Win32::Mutex->open($name2)) ) {
    sleep(0.1);
}

print STDERR "\n> ";

while (<STDIN>) {
    last if m/^\s*exit\s*$/i;
    if (!/^\s*next\s*$/i) {
        print STDERR "Invalid command $_";
        next;
    }
    #if ($process->Wait(0) == 1) {;
    #    warn "Target process terminated.\n";
    #    last;
    #}
    $mutex->release;
    $mutex = Win32::Mutex->open($name) or
        die "Can't open the mutex object!\n";
    $mutex->wait;
 
    # sleep(1);
} continue {
    print STDERR "> ";
}

#$process->Kill(0);

END {
    $mutex->release;
}

0;

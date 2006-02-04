#: svk-ci.pl
#: drop-in replacement (and also a wrapper) for `svk ci'
#: Copyright (c) 2006 Agent Zhang
#: 2006-02-04 2006-02-04

use strict;
use warnings;

use File::Copy;
use Date::Simple ('today');

my $DatePat = qr/\d{4}-\d{1,2}-\d{1,2}/o;
my $Fatals = 0;

{
    open my $in, 'svk status |' or
        die "error: Can't spawn `svk status': $!";
    while (<$in>) {
        if (/^M\s+(\S+)/o) {
            my $file = $1;
            print "info: checking file $file...\n";
            process_file($file);
        }
    }
    close $in;

    if ($Fatals) {
        die "\nFor total $Fatals fatal errors. Commit Stop.\n";
    }
    system('svk ci');
}

sub process_file {
    my $file = shift;
    if ($file =~ /\.(?:pl|pm|t)$/o or $file =~ /^[^\.]$/o) {
        process_pl($file);
    }
    dos2unix($file);
}

sub process_pl {
    my $file = shift;
    open my $in, $file or
        die "error: Can't open $file for reading: $!";
    my $today_dt  = today();
    my $out_of_date = 0;
    while (<$in>) {
        last if /^[^\#]/o;
        if (/^ \#: \s* ($DatePat) \s+ ($DatePat) \s* $/ox) {
            my ($a, $b) = ($1, $2);
            my $create_dt = new_date($file, $a);
            print "  Create:         $create_dt\n";
            my $update_dt = new_date($file, $b);
            print "  Last Modified:  $update_dt\n";
            print "  Today:          $today_dt\n\n";
            if ($create_dt > $update_dt) {
                log_error(
                    $file,
                    "Create Date is more recent than Last Modified Date",
                );
                last;
            } elsif ($update_dt > $today_dt) {
                log_error(
                    $file,
                    "Last Modified Date is more recent than today",
                );
                last;
            } elsif ($update_dt < $today_dt) {
                log_error(
                    $file,
                    "Last Modified Date is not today",
                );
                $out_of_date = 1;
                last;
            }
        }
    }
    close $in;
    if ($out_of_date) {
        # Updating existing file may flush the history cache of 
        # user's editor unintentionally, so the following line
        # is currently commented out.

        #update_file($file, $today_dt);
    }
}

sub new_date {
    my ($file, $s) = @_;
    $s =~ s/-(\d)-/-0$1-/og;
    $s =~ s/-(\d)$/-0$1/o;
    my $date;
    eval { $date = Date::Simple->new($s); };
    die "error: $file: $.: $@" if $@;
    return $date;
}

sub update_file {
    my ($file, $today) = @_;

    my ($tmp, $tmpfile) = tempfile();
    open my $in, $file or
        die "error: Can't open $file for reading: $!";
    while (<$in>) {
        s/^ ( \#: \s* $DatePat ) \s+ $DatePat \s* \n/$1 $today\n/ox;
        print $tmp $_;
    }
    close $in;
    close $tmp;

    copy($tmpfile, $file) or
        die "copying from $tmpfile to $file failed: $!";
}

sub dos2unix {
    my $file = shift;
    if ($^O eq 'MSWin23' and -T $file) {
        system("dos2unix $file");
    }
}

sub log_error {
    my ($file, $msg) = @_;
    warn "* $file: line $.: error: $msg.\n";
    $Fatals++;
}

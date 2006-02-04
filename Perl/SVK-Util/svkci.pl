#: svkci.pl
#: drop-in replacement (and also a wrapper) for `svk ci'
#: Copyright (c) 2006 Agent Zhang
#: 2006-02-04 2006-02-04

use strict;
use warnings;

use File::Basename;
use File::Copy;
use Date::Simple ('today');

my $DatePat = qr/\d{4}-\d{1,2}-\d{1,2}/o;
my $Fatals = 0;
my @Errors = ();

my $SVK;

BEGIN {
    $SVK = $ENV{SVK_PATH} || 'svk';
}

sub log_error (@) {
    my $file = shift;
    my $msg  = join('', @_);
    push @Errors, "$file: line $.: error: $msg.";
    $Fatals++;
}

sub new_date ($$) {
    my ($file, $s) = @_;
    $s =~ s/-(\d)-/-0$1-/og;
    $s =~ s/-(\d)$/-0$1/o;
    my $date = Date::Simple->new($s);
    log_error($file, "Date $s is invalid")
        if not defined $date;
    return $date;
}

sub check_dates ($$$) {
    my ($file, $c, $u) = @_;
    my $create_dt = new_date($file, $c);
    return undef if not defined $create_dt;

    my $update_dt = new_date($file, $u);
    return undef if not defined $update_dt;

    my $pass = 1;
    if ($create_dt > $update_dt) {
        $pass = 0;
        log_error(
            $file,
            "Create Date ($create_dt) is more recent than ",
            "Last Modified Date ($update_dt)",
        );
    }
    my $today_dt  = today();
    if ($update_dt > $today_dt) {
        $pass = 0;
        log_error(
            $file,
            "Last Modified Date ($update_dt) is more recent ",
            "than today ($today_dt)",
        );
    }
    if ($update_dt < $today_dt) {
        $pass = 0;
        log_error(
            $file,
            "Last Modified Date ($update_dt) is not today ",
            "($today_dt)",
        );
    }
    return $pass;
}

sub process_pl ($) {
    my $file = shift;
    open my $in, $file or
        die "error: Can't open $file for reading: $!";
    my $first = 1;
    while (<$in>) {
        last if /^[^\#]/o;
        if ($first and m{^ \#: \s* (\S+) \s* $}xo) {
            my $fname = $1;
            #warn "File Name: $fname\n";
            if (basename($fname) ne basename($file)) {
                log_error($file, "File name ($fname) malformed");
            }
            $first = 0;
        } elsif (/^ \#: \s* Copyright \s+ (.+) /xio) {
            # ...
            my $s = $1;
            my $year = today()->year();
            #warn "YEAR: $year";
            if ($s !~ /$year/) {
                log_error($file, "Copyright out-of-date. It's $year already");
            }
            $first = 0;
        } elsif (/^ \#: \s* ($DatePat) \s+ ($DatePat) \s* $/ox) {
            last if not check_dates($file, $1, $2);
        } elsif ($first and /^\#:/) {
            $first = 0;
        }
    }
    close $in;
}

sub dos2unix ($) {
    my $file = shift;
    if ($^O eq 'MSWin32' and -T $file) {
        print STDERR "dos2unix: ";
        system("dos2unix $file");
    }
}

sub process_file {
    my $file = shift;
    process_pl($file);
    dos2unix($file);
}

open my $in, "$SVK status |" or
    die "error: Can't spawn `svk status': $!";
while (<$in>) {
    if (/^[MA]\s+(\S+)/o) {
        my $file = $1;
        print "info: checking file $file...\n";
        process_file($file) if -f $file;
    }
}
close $in;

if ($Fatals) {
    print "\n";
    warn "* $_\n" for (@Errors);
    my $noun = $Fatals > 1 ? 'errors' : 'error';
    die "\nFor total $Fatals fatal $noun. Commiting Stop.\n";
}

system("$SVK ci");

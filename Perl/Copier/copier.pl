#: copier.pl
#: Win32 backup script
#: 2003-11-08 2005-01-13

use strict;
use warnings;

use Getopt::Std;
use File::Find;
use File::Spec;

my %opts;
getopts('tq', \%opts);  # Trim Mode And Quiet Mode

sub Usage {
    my $retval = shift;
    my $info = "Usage:\n\t perl -w copier.pl <source-path> <destination-path>\n";
    if ($retval == 0) {
        print $info;
        exit(0);
    } else {
        warn $info;
        exit($retval);
    }
}

if (@ARGV != 2){
	Usage(1);
}

my ($src, $des) = @ARGV;
$src =~ s,/,\\,g;
$des =~ s,/,\\,g;
system qq[ Xcopy "$src" "$des" /D /E /Y /F /I ];

exit(0) if not defined $opts{'t'};

if (-d $des and -d $src) {
    $des =~ s/\\$//g;
	process_dir($des, $src);
}

sub process_dir {
	my ($des_dir, $src_dir) = @_;
    #warn "$des_dir :: $src_dir";
    return if $des_dir =~ /\.svn/;
    if (-d $des_dir and not -e $src_dir) {
        warn "Removing $des_dir...\n";
        system qq[ RD /S "$des_dir" ];
        return;
    }
    opendir my $dir, $des_dir
        or die "can't open dir $des_dir for reading: $!";
    my @entries = readdir $dir;
    close $dir;
    foreach my $entry (@entries) {
        #warn "$entry";
        next if $entry eq '.' or $entry eq '..';
        my $path = "$des_dir\\$entry";
        if (-f $path) {
            process_file($path, "$src_dir\\$entry");
        } elsif (-d $path) {
            process_dir($path, "$src_dir\\$entry");
        } else {
            warn "unknown entity $path";
        }
    }
}

sub process_file {
    my ($des_file, $src_file) = @_;
    if (-f $des_file and not -e $src_file) {
        warn "Removing $des_file...\n";
        if (system( qq[ del /P "$des_file" ] ) != 0) {
            system qq[ del /P /AH "$des_file" ];
        }
    }
}

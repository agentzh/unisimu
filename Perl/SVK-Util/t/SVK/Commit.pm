#: t/SVK/Commit.pm
#: 2006-02-04 2006-02-04

package t::SVK::Commit;

use Test::Base -Base;
use IPC::Cmd;
use Test::More;
use File::Temp qw(tempfile tempdir);
use Carp qw(confess);
use FindBin;
use Cwd;

BEGIN {
    $ENV{SVK_PATH} = "$^X svk";
}

filters {
    stderr      => [qw<filter_date>],
    stderr_like => [qw<filter_date>],
    stdout      => [qw<filter_date>],
    stdout_like => [qw<filter_date>],
    content     => [qw<filter_date>],
};

my $script = "$FindBin::Bin/../../svkci.pl";

our @EXPORT = qw(
    run_test run_tests
);

sub process_pre ($) {
    my $block = shift;
    my $code = $block->pre;
    return if not $code;
    {
        package main;
        eval $code;
    }
    confess "error in `pre' section: $@" if $@;
}

sub process_post ($) {
    my $block = shift;
    my $code = $block->post;
    return if not $code;
    {
        package main;
        eval $code;
    }
    confess "error in `post' section: $@" if $@;
}

sub compare ($$$) {
    my ($got, $expected, $desc) = @_;
    return if not defined $expected;
    if ($desc =~ /\w+_like/) {
        Test::More::like($got, qr/^$expected$/ms, $desc);
    } else {
        Test::More::is($got, $expected, $desc);
    }
}

sub join_list (@) {
    my @args = @_;
    for (@args) {
        if (ref $_ eq 'ARRAY') {
            $_ = join('', @$_);
        }
    }
    return wantarray ? @args : $args[0];
}

sub test_shell_command ($$@) {
    my $block    = shift;
    my $cmd      = shift;
    my %filters  = @_;
    return if not defined $cmd;

    my ($success, $errcode, $full, $stdout, $stderr)
        = join_list IPC::Cmd::run( command => $cmd );

    my $errcode2 = $block->error_code;
    if ($errcode2 and $errcode2 =~ /\d+/s) {
        $errcode2 = $&;
    }

    my $success2 = $block->success;
    if ($success2 and $success2 =~ /\w+/s) {
        $success2 = lc($&);
    }

    my $name = $block->name;

    while (my ($key, $val) = each %filters) {
        if ($key =~ /stdout|stderr/) {
            next if ref $val eq 'CODE';
            no strict 'refs';
            my $arg = ${"$key"};
            next if not defined $arg;
            $val->($arg);
        }
    }

    compare $stdout, $block->stdout, "stdout - $name";
    compare $stdout, $block->stdout_like, "stdout_like - $name";
    compare $stderr, $block->stderr, "stderr - $name";
    compare $stderr, $block->stderr_like, "stderr_like - $name";
    compare $errcode, $errcode2, "error_code - $name";
    compare (
        $success ? 'true' : 'false',
        $success2,
        "success - $name",
    );
    if (not defined $block->stderr() and
            not defined $block->stderr_like() and
            $stderr) {
        warn $stderr;
    }
}

sub create_file ($$) {
    my ($filename, $content) = @_;
    my $fh;
    if (not $filename) {
        ($fh, $filename) = 
            tempfile( "svkci_XXXXX", DIR => '.', UNLINK => 1 );
    } else {
        open $fh, "> $filename" or
            confess("can't open $filename for writing: $!");
    }
    print $fh $content;
    close $fh;
    return $filename;
}

sub gen_svk ($) {
    my $output = shift;
    chomp $output;
    create_file('svk', <<"_EOC_");
#: svk
#: Stub script for `svk'

my \$cmd = shift;
if (\$cmd eq 'status') {
    print <<'.';
$output
.
}
warn "svk \$cmd invoked.\n";
_EOC_
    return;
}

sub run_test ($) {
    my $block = shift;

    my $dir = tempdir('svkci_XXXXXX', TMPDIR => 1, CLEANUP => 1);
    #warn "TEMP_DIR: $dir\n";
    my $saved_cwd = Cwd::cwd;

    chdir $dir;

    my $filename;
    my $content = $block->content;
    if (defined $content) {
        $filename = create_file($block->filename, $content);
    }

    my $status = $block->svk_status;
    #warn "++++++++++++++ $status +++++++";
    if (defined $status) {
        gen_svk($status);
    } else {
        gen_svk("M  $filename\n");
    }

    process_pre($block);

    my $cmd = [$^X, $script];
    test_shell_command(
        $block, $cmd,
    );

    process_post($block);

    chdir $saved_cwd;
}

sub run_tests () {
    for my $block (blocks) {
        run_test($block);
    }
}

package t::SVK::Commit::Filter;

use Test::Base::Filter -Base;
use Date::Simple qw(today);

sub filter_date {
    my $s = shift;
    my $today = today();
    $s =~ s/\^today\^/$today/gsie;
    $s =~ s/\^tomorrow\^/$today+1/gsie;
    $s =~ s/\^yesterday\^/$today-1/gsie;
    $s =~ s/\^year\^/$today->year/gsie;
    return $s;
}

1;

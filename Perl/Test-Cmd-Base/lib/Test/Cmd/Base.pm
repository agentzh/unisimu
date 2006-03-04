#: Test/Cmd/Base.pm
#: Coyright (c) 2006 Agent Zhang
#: 2006-02-27 2006-03-04

package Test::Cmd::Base;

use strict;
use warnings;

use Test::Base -Base;
use Text::Balanced qw( gen_delimited_pat );
use IPC::Run3 'run3';
use File::Temp qw(tempfile tempdir);
use Carp qw(croak);
use FindBin;
use Cwd;

our $VERSION = '0.01';

our @EXPORT = qw(
    run_cmd_tests
);

our (@Exe, %Filters, $PWD, $INFILE, $Script, $USE_TEMP_DIR);

filters_delay();
our $DelimPat = gen_delimited_pat(q{"});

sub use_temp_dir {
    $USE_TEMP_DIR = 1;
}

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

sub process_found ($) {
    my $block = shift;
    my $buf = $block->found;
    return if not $buf;
    my @files = split /\s+/s, $buf;
    for my $file (@files) {
        Test::More::ok(
            (-f $file), 
            "File $file should be found - ".$block->name
        );
    }
}

sub process_not_found ($) {
    my $block = shift;
    my $buf = $block->not_found;
    return if not $buf;
    my @files = split /\s+/s, $buf;
    for my $file (@files) {
        Test::More::ok(
            !(-f $file),
            "File $file should NOT be found - ".$block->name
        );
    }
}

sub compare ($$$) {
    my ($got, $expected, $desc) = @_;
    return if not defined $expected;
    $got =~ s/\r\n/\n/gs;
    if ($desc =~ /\w+_like/) {
        Test::More::like($got, qr/$expected/, $desc);
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

    my $stdin = $block->stdin;
    my ($stdout, $stderr);
    run3($cmd, \$stdin, \$stdout, \$stderr);
    my $errcode = $?;
    my $success = ($errcode == 0);

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
        #warn "$key $val";
        if ($key eq 'stdout') {
            $stdout = $val->($stdout);
        } elsif ($key eq 'stderr') {
            $stderr = $val->($stderr);
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
            tempfile( "create_file_XXXXX", DIR => '.', UNLINK => 1 );
    } else {
        open $fh, "> $filename" or
            confess("can't open $filename for writing: $!");
    }
    #$content .= "\n\nSHELL=$SHELL" if $SHELL;
    print $fh $content;
    close $fh;
    return $filename;
}

sub process_touch ($) {
    my $block = shift;
    my $buf = $block->touch;
    return if not $buf;
    touch(split /\s+/, $buf);
}

sub process_utouch ($) {
    my $block = shift;
    my $buf = $block->utouch;
    return if not $buf;
    utouch(split /\s+/, $buf);
}

sub run_cmd_test ($) {
    my $block = shift;

    my ($saved_cwd);
    if ($USE_TEMP_DIR) {
        my $tempdir = tempdir( 'backend_XXXXXX', TMPDIR => 1, CLEANUP => 1 );
        $saved_cwd = Cwd::cwd;
        chdir $tempdir;
    }
    $PWD = Cwd::cwd;
    $PWD =~ s,\\,/,g;

    my $filename = $block->filename;
    chomp $filename if $filename;
    my $source   = $block->source;

    if (defined $source) {
        my $fh;
        if (not $filename) {
            ($fh, $filename) = 
                tempfile( "Makefile_XXXXX", DIR => '.', UNLINK => 1 );
        } else {
            open $fh, "> $filename" or
                confess("can't open $filename for writing: $!");
        }
        $INFILE = $filename;
        $INFILE =~ s,\\,/,g;
        $block->run_filters;
        print $fh $block->source;
        close $fh;
    } else {
        $block->run_filters;
        $filename = $block->filename;
    }

    process_pre($block);
    process_touch($block);
    process_utouch($block);

    run_cmd($block, $filename);

    process_post($block);
    process_found($block);
    process_not_found($block);

    if ($USE_TEMP_DIR) {
        chdir $saved_cwd;
    }
    #warn "\nstderr: $stderr\nstdout: $stdout\n";
}

sub run_cmd($$) {
    my ($block, $filename) = @_;
    my $options  = $block->options || '';
    my $goals    = $block->goals || '';

    @Exe = split_arg($Script) if not @Exe;
    my @args = @Exe;
    #warn Dumper($filename);
    my $cmd = [ @args, process_args("$options $goals") ];
    #warn Dumper($cmd);
    test_shell_command( $block, $cmd, %Filters );
}

sub run_cmd_tests ($) {
    $Script = shift;
    if (not $Script) {
        croak "run_cmd_tests: No command specified";
    }
    for my $block (blocks()) {
        run_cmd_test($block);
    }
}

sub touch (@) {
    utouch(0, @_);
}

# Touch with a time offset.  To DTRT, call touch() then use stat() to get the
# access/mod time for each file and apply the offset.

sub utouch (@) {
    my $off = shift;
    my @files = @_;
    foreach my $file (@files) {
        my $in;
        open $in, ">>$file" or
            print $in '' or close $in or
            die "Can't touch $file: $!";
    }
    my (@s) = stat($files[0]);
    utime($s[8] + $off, $s[9] + $off, @files);
}

sub extract_many (@) {
    my $text = shift;
    my @flds;
    while (1) {
        #warn '@flds = ', Dumper(@flds);
        if ($text =~ /\G\s* ( ; | >>? | < | \|\| | \&\& ) /gcox) {
            push @flds, $1;
        } elsif ($text =~ /\G\s* ( (?:\\.)+ [^'";><\|\&\s]* )/gcox) {
            push @flds, $1;
        } elsif ($text =~ /\G\s*('[^']*')/gco) {
            push @flds, $1;
        } elsif ($text =~ /\G\s*($DelimPat)/gco) {
            push @flds, $1;
        } elsif ($text =~ /\G\s*( \S (?:[^ ; > < ' " \s \\ \| \& ]|\\.)* )/gcox) {
            push @flds, $1;
        } else {
            last;
        }
    }
    return @flds;
}

sub split_arg ($) {
    my $text = shift;
    return () if not defined $text;
    return extract_many($text);
}

sub process_escape (@) {
    return if $_[0] !~ /\\/;
    my $list = quotemeta $_[1];
    $_[0] =~ s/\\[$list]/substr($&,1,1)/eg;
}

sub process_args ($) {
    my $text = shift;
    my @args = split_arg($text);
    foreach (@args) {
        #warn "----------\n";
        #warn Dumper(@args, $_);
        #warn "----------\n";
        if (/^"(.*)"$/) {
            #warn "---------";
            #warn qq{Pusing "$1" into args\n};
            $_ = $1;
            process_escape( $_, q{"\\$@\#} );
        } elsif (/^'(.*)'$/) {
            #warn "  Pusing '$1' into args\n";
            $_ = $1;
        }
    }
    return @args;
}

1;
__END__

=head1 NAME

Test::Cmd::Base - Test::Cmd powered by Test::Base

=head1 AUTHOR

Agent Zhang L<mailto:agentzh@gmail.com>


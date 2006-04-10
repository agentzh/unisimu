package Pod::Extend;

use strict;
use warnings;

our $VERSION = '0.01';

use base 'Pod::Parser';

use Perl6::Slurp;
use File::Spec;
use Text::Table;
use Scalar::Util qw/looks_like_number/;
#use Smart::Comments;

our $maple;
our $dbi_dsn;
our $dbi_username;
our $dbi_password;

my $in_code = 0;
my $code = '';
my %hooks = (
    perl => sub {
        my $code = shift;

        package Pod::Extend::Temp;
        no strict;
        $Pod::Extend::Temp::maple = $Pod::Extend::maple;

        $Pod::Extend::Temp::code = $code;

        $res = '';
        tie_output(*STDOUT, $buffer);
        eval $code;
        untie *STDOUT;

        $code = $Pod::Extend::Temp::code;

        $res = $my_stdout;
        package Pod::Extend;
        local $" = ',';
        if ($@) {
            if ($res) {
                $res = $@ . $res;
            } else {
                $res = $@;
            }
        }
        return fmt_code_res($code, $res, 'Perl');
    },
    maple => sub {
        my $code = shift;
        require 'PerlMaple.pm';
        $maple ||= PerlMaple->new;
        my $res = $maple->eval_cmd($code);
        $res = defined $res ? $res : $maple->error;
        return fmt_code_res($code, $res, 'Maple');
    },
    include => sub {
        my $file = shift;
        $file =~ s/^[<"]|[>"]$//g;
        my $content = slurp $file;
        return fmt_code_res($content);
    },
    shell => sub {
        my $cmd = shift;
        ### $cmd
        return if not $cmd;
        my $code = "    \$ $cmd\n";
        #warn $cmd;
        my $res;
        if ($cmd =~ m/>\s*[^"'>]+\s*(?:2>\&1)?\s*$/) {
            system ($cmd);
        } else {
            my $tmpdir = File::Spec->tmpdir;
            my $tmpfile = "$tmpdir/prepro.tmp";
            if (-f $tmpfile) {
                unlink $tmpfile or die "Can't unlink $tmpfile.\n";
            }
            system("$cmd > $tmpfile 2>&1");
            open my $tmp, $tmpfile or
                die "Can't open $tmpfile for reading: $!";
            while (my $line = <$tmp>) {
                $code .= "    $line";
            }
            close $tmp;
            unlink $tmpfile;
        }
        return $code . "\n\n";
    },
    sql => sub {
        require 'DBI.pm';
        my $sql = shift;
        ### $sql
        $sql =~ s/^[\s\n]+|[\s\n]+$//sg;
        my $res = '';

        my $dbh = connect_db();
        if (not $dbh) {
            return fmt_code_res($sql, $DBI::errstr, 'SQL')
        }

        my $sth = $dbh->prepare($sql);
        if (not $sth) {
            my $output = fmt_code_res($sql, $DBI::errstr, 'SQL');
            $dbh->disconnect;
            return $output;
        }

        my $rv = $sth->execute() or $res .= "$DBI::errstr\n\n";
        if (!$sth->{'NUM_OF_FIELDS'}) { # not a select statement
            local $^W=0;
            if (not defined $rv) {
                $rv = "undefined number of";
            } elsif ($rv == -1) {
                $rv = "unknown number of";
            }
            $res .= "[$rv row".
                (looks_like_number($rv) && $rv==1 ? "" : "s").
                " affected]\n";
            $dbh->disconnect;
            return fmt_code_res($sql, $res, 'SQL');
        }

        my @flds = @{ $sth->{NAME} };
        my $tb = Text::Table->new(@flds);
        while (my $row = $sth->fetchrow_arrayref) {
            $tb->add(@$row);
        }
        $dbh->disconnect;

        $res = $tb->rule('-', '+').
               $tb->title.
               $tb->rule('-', '+').
               $tb->body.
               $tb->rule('-');
        return fmt_code_res($sql, $res, 'SQL');
    },
);

sub connect_db {
    die "No env DSN set.\n" unless $dbi_dsn;
    my $dbh;
    if ($dbi_username) {
        $dbh = DBI->connect($dbi_dsn, $dbi_username, $dbi_password, { PrintError => 1 });
    } else {
        $dbh = DBI->connect($dbi_dsn, { PrintError => 1 });
    }
    return $dbh;
}

sub fmt_code_res {
    my ($code, $res, $lang) = @_;
    #warn "\n-------\n";
    #warn "$code\n";
    #warn "-------\n\n";
    $code =~ s/^/    /smg;
    $res = '' if not defined $res;
    $res =~ s/^/    /smg;
    $code =~ s/^\n+|[\s\n]+$//gs;
    $res  =~ s/^\n+|[\s\n]+$//gs;
    $lang = $lang ? "I<$lang>\n\n" : '';
    if ($res) {
        return "$lang$code\n\nI<Output>:\n\n$res\n\n";
    } elsif ($lang) {
        return "$lang$code\n\nI<No Output>\n\n";
    } else {
        return "$lang$code\n\n";
    }
}

sub command {
    my $self = shift;
    my ($command, $paragraph, $lineno) = @_;
    $command = lc($command);
    #$" = ':';
    #warn "@_";
    my $hook = $hooks{$command};
    $paragraph =~ s/^[\s\n]+|[\s\n]+$//gs;
    if ($hook) {
        #warn "HERE";
        $self->output( $hook->($paragraph) );
        return;
    }
    $paragraph = lc($paragraph);
    $hook = $hooks{$paragraph};
    if ($hook) {
        if ($command eq 'begin') {
            #warn "BEGIN";
            if ($in_code) {
                die "'=end $paragraph' missing at line $lineno\n";
            }
            $code = '';
            $in_code = 1;
        } elsif ($command eq 'end') {
            if (not $in_code) {
                die "'=end $paragraph' unexpected at line $lineno\n";
            }
            $code =~ s/^[\s\n]+|[\s\n]+$//gs;
            $self->output( $hook->($code) );
            $in_code = 0;
        }
    } else {
        $self->SUPER::command(@_);
    }
}

sub verbatim {
    my $self = shift;
    my ($paragraph, $line_num) = @_;
    if ($in_code) {
        $code .= $paragraph;
        #warn $code;
        return;
    }
    $self->SUPER::verbatim(@_);
}

sub textblock {
    my $self = shift;
    my ($paragraph, $line_num) = @_;
    if ($in_code) {
        $code .= $paragraph;
        #warn $code;
        return;
    }
    $self->SUPER::textblock(@_);
}

# Output text to the output device.
sub output { print { $_[0]->output_handle } $_[1] }

sub tie_output {
    my $handle = shift;
    die "No buffer to tie" unless @_;
    tie $handle, 'Pod::Extend::Handle', $_[0];
}

package Pod::Extend::Handle;

sub TIEHANDLE() {
    my $class = shift;
    bless \ $_[0], $class;
}

sub PRINT {
    $$self .= $_ for @_;
}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

Pod::Extend - Perl extension for Pod extension

=head1 SYNOPSIS

  use Pod::Extend;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for Pod::Extend, created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.

Blah blah blah.

=head2 EXPORT

None by default.



=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 AUTHOR

A. U. Thor, E<lt>a.u.thor@a.galaxy.far.far.awayE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by A. U. Thor

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.


=cut

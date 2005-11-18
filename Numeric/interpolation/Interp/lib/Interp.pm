#: Interp.pm
#: Base class for interpolating classes
#: v0.01
#: Copyright (c) Agent Zhang
#: 2005-11-17 2005-11-18

package Interp;

use strict;
use warnings;
use PerlMaple;

our $VERSION = '0.01';

our $error;
our $maple;

BEGIN {
    $maple = PerlMaple->new;
    $maple->eval('interface(prettyprint=false)');
}

sub new {
    my $class = shift;
    my %data = @_;
    my (@x, @y);
    if ($data{Xs} and $data{Ys}) {
        @x = @{$data{Xs}};
        @y = @{$data{Ys}};
    } else {
        for (sort keys %data) {
            push @x, $_;
            push @y, $data{$_};
        }
    }
    return undef if not $class->valid_xs(@x);
    my $self = {
        x => \@x,
        y => \@y,
    };
    return bless $self, $class;
}

sub Xs {
    return @{shift->{x}};
}

sub Ys {
    return @{shift->{y}};
}

sub polynomial { die "This is only a stub method"; }

sub test_polynomial {
    my $self = shift;
    my $poly = shift;
    my @Xs = $self->Xs;
    my @Ys = $self->Ys;
    foreach (0..@Xs-1) {
        my $res = $maple->eval(
            "testeq(eval($poly, x=$Xs[$_]), $Ys[$_]);"
        );
        if ($res =~ /bytes used=/) {
            warn " !!! $res\n";
        }
        if (!defined $res or $res ne 'true') {
            #warn "test: $res\n";
            #warn $maple->error, "\n" if $maple->error;
            return undef;
        }
    }
    return 1;
}

sub error {
    if ($maple->error) {
        return "Maple: ".$maple->error;
    } else {
        return $error;
    }
}

sub maple {
    return $maple;
}

sub valid_xs {
    shift;
    my @xs = @_;
    @xs = sort { $a cmp $b } @xs;
    for (my $i = 0; $i < @xs-1; $i++) {
        if ($xs[$i+1] eq $xs[$i]) {
            $error = "Redundant X";
            return undef;
        }
    }
    return 1;
}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

Interp - Perl extension for blah blah blah

=head1 SYNOPSIS

  use Interp;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for Interp, created by h2xs. It looks like the
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

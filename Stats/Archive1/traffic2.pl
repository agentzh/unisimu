#: traffic2.pl
#: Example A of Section 1.2, Page 2-3
#: Require:
#:   Set::Scalar
#: Agent2002. All rights reserved.
#: 2005-03-06 2005-03-07

use strict;
use warnings;
use Set::Scalar;

my $NELEMS = 3;
my @Res;
sub genres {
    my ($pred, $buf) = @_;
    unless (defined $buf) {
        $buf = '';
        @Res = ();
    }
    if (length($buf) == $NELEMS) {
        push @Res, $buf
            if (!$pred or &$pred($buf));
        return;
    }
    genres($pred, $buf.'c');
    genres($pred, $buf.'s');
}

$" = ', ';

genres;
my @Omega = @Res;
my $Omega = Set::Scalar->new(@Omega);
print "Omega = { @Omega }\n";

genres( sub { $_[0] =~ m/^s/ } );
my @A = @Res;
my $A = Set::Scalar->new(@A);
print "A = { @A }\n";

genres( sub { $_[0] =~ m/^..s/ } );
my @B = @Res;
my $B = Set::Scalar->new(@B);
print "B = { @B }\n";

genres( sub { $_[0] =~ m/^s|^..s/ } );
print "Union: C = { @Res }\n";

my $C = $A + $B;
my @C = $C->elements;
print "Union: C = { @C }\n";

genres( sub { $_[0] =~ m/^s.s/ } );
print "Intersection: C = { @Res }\n";

$C = $A * $B;
@C = $C->elements;
print "Intersection: C = { @C }\n";

genres( sub { $_[0] !~ m/^s/ } );
print "Complement of A: { @Res }\n";

$C = $Omega - $A;
@C = $C->elements;
print "Complement of A: { @C }\n";

__END__

We are often interested in particular subsets of Omega, which
in probability language are called events. In Example A(see
traffic.pl), the event that the commuter stops at the first
light is the subset of Omega denoted by

    A = { sss, ssc, scc, scs }

The union of two events, A and B, is the event C that either
A occurs or B occurs or both occur: C = A ¡È B. For example,
if A is the event that the commuter stops at the first light
(listed before) and if B is the event that she stops at the
third light,

    B = { sss, scs, ccs, css }

then C is the event that she stops at the first light or stops
at the third light and consists of the outcomes that are in A
or in B or in both:

    C = { sss, ssc, scc, scs, ccs, css }

The intersection of two events, C = A ¡É B, is the event that
both A and B occur. If A and B are as listed previous, then C
is the event that the commuter stops at the first light and
stops at the third light and thus consists of those outcomes
that are common to both A and B:

    C = { sss, scs }

The complement of an event, A^c, is the event that A does not
occur and thus consists of all those elements in the sample
space that are not in A. The complement of the event that the
commuter stops at the first light is the event that she continues
at the first light:

    A^c = { ccc, ccs, css, csc }

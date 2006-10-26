use strict;
use warnings;

use Getopt::Std;
use PPI;
use File::Spec;
use Class::Inspector;
use List::MoreUtils 'any';

my $public_only;

my %opts;
getopts('p', \%opts);
if ($opts{p}) {
    $public_only = 1;
}
warn "only public methods are displayed." if $public_only;

my @pmfiles = sort map { -d $_ ? all_in($_) : $_ } map glob, @ARGV;
warn join "\n", @pmfiles, "\n";

my %pkg;
for my $pmfile (@pmfiles) {
    my $doc = PPI::Document->new( $pmfile );
    if (!$doc) {
        warn "Can't parse $pmfile.\n";
        next;
    }
    my $res = $doc->find('PPI::Statement::Package');
    next if !$res;
    my @pkg = map { $_->namespace } @$res;
    #warn "@pkg";
    for my $pkg (@pkg) {
        if ($pkg{$pkg}) { next; }
        $pkg{$pkg} = 1;
        if (! Class::Inspector->loaded($pkg)) {
            require $pmfile;
        }

        print "$pkg\n";
        my $func = Class::Inspector->functions($pkg);
        if ($func and @$func) {
            if ($public_only) {
                @$func = grep { /^[^_]/ } @$func;
            }
            if (!@$func) { print "-\n"; } else { print "@$func\n"; }
        } else {
            print "-\n";
        }
        print "-\n";
        my $subclasses = Class::Inspector->subclasses($pkg);
        if (!$subclasses) {
            print "-\n\n";
        } else {
            no strict 'refs';
            my @child = grep {
                #warn "!!!! ", join ' ', @{"${_}::ISA"};
                any { $_ eq $pkg } @{"${_}::ISA"};
            } @$subclasses;
            if (!@child) { print "-\n\n"; } else { print "@child\n\n"; }
        }
    }
}

sub all_in {
    my $start = shift;

    my @hits = ();

    local *DH;
    if ( opendir( DH, $start ) ) {
        my @files = sort readdir DH;
        closedir DH;
        for my $file ( @files ) {
            #warn $file;
            next if $file eq File::Spec->updir || $file eq File::Spec->curdir;
            next if $file eq ".svn";
            next if $file eq "CVS";
            next if -f $file && $file !~ m/\.pm$/i;

            my $currfile = File::Spec->catfile( $start, $file );
            if ( -d $currfile ) {
                push @hits, all_in( $currfile );
            } else {
                push @hits, $currfile;
            }
        }
    } else {
        warn "$start: $!\n";
    }

    return @hits;
}

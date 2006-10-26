use strict;
use warnings;

use Getopt::Std;
use Class::Inspector;
use List::MoreUtils 'any';

my %opts;
getopts('p', \%opts);
my $public_only = $opts{p};

my ($module, $pattern) = @ARGV;
$pattern = '' if !defined $pattern;

#warn $module;
if ($module) {
    eval "use $module;";
    if ($@) { die $@ }
}

#warn join ' ', keys %::PPI::;

my @pkg = grep { /$pattern/ } map { s/^::|::$//g; $_ } get_packages();
warn "@pkg";

for my $pkg (@pkg) {
    #$pkg =~ s/::::/::/g;
    print "$pkg\n";
    my $func = Class::Inspector->functions($pkg, public => $public_only);
    if ($func and @$func) {
        print "@$func\n";
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
            warn "!!!! ", join ' ', @{"${_}::ISA"};
            any { $_ eq $pkg } @{"${_}::ISA"};
        } @$subclasses;
        print "@child\n\n";
    }
}

sub get_packages {
    no strict 'refs';
    my $pkg_name = shift || '::';
    my $cache = shift || {};
    return if $cache->{$pkg_name};
    $cache->{$pkg_name} = 1;
    for my $entry (keys %$pkg_name) {
        next if $entry !~ /\:\:$/ or $entry eq 'main::';
        my $subpkg_name = $pkg_name.$entry;
        #warn $subpkg_name;
        get_packages($subpkg_name, $cache);
        $cache->{$subpkg_name} = 1;
    }
    wantarray ? keys %$cache : $cache;
}

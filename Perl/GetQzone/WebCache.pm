package WebCache;

use strict;
use warnings;

use Storable qw( retrieve store );

our $RefreshCache = 0;
our $CacheFile = 'MyCache.dat';
my $MAX_CACHED = 3;

sub new {
    my $class = shift;
    my $self;
    if (!-f $CacheFile) {
        warn "  warning: cache file $CacheFile doesn't exist.\n";
        $self = {};
    } else {
        $self = retrieve($CacheFile);
        if (!$self) {
            warn "  warning: cache file $CacheFile is empty.\n";
            $self = {};
        } else {
            warn "  info: cache file $CacheFile loaded.\n";
        }
    }
    $self->{__COUNTER__} = 0;
    bless $self, $class;
    return $self;
}

sub set {
    my $self = shift;
    my $key = shift;
    #warn $key;
    my $value = shift; # an HTTP::Response
    $self->{$key} = $value;
    if (++$self->{__COUNTER__} >= $MAX_CACHED) {
        warn "  info: updating cache file $CacheFile...\n";
        delete $self->{__COUNTER__};
        store $self, $CacheFile;
        $self->{__COUNTER__} = 0;
    }
}

sub get {
    my $self = shift;
    my $key = shift;
    #warn $key;
    return undef if $RefreshCache;
    #if ($self->{$key}) {
        #warn "  info: using cached response...\n";
    #}
    return $self->{$key};
}

sub DESTROY {
    my $self = shift;
    warn "  info: updating cache file $CacheFile...\n";
    store $self, $CacheFile;
}

1;

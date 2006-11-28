package Qooqle::Model::MessageCollection;

use strict;
use warnings;
use base 'Jifty::Collection';

use Qooqle::Model::Message;
use Regexp::Common;
use Data::Record;

my $DataRecord;

sub split_keys ($$) {
    my ($self, $keys) = @_;
    $DataRecord ||= Data::Record->new({
        split  => qr/\s+/,
        unless => $RE{quoted},
        trim   => 1,
    });
    my @keys = grep { $_ ne '' } $DataRecord->records($keys);
    map { s/^['"]|['"]$//g; s/\\"/"/g; s/\\'/'/g; } @keys;
    \@keys;
}

sub search ($$) {
    my ($self, $keys) = @_;
    if (!ref $keys) {
        $keys = $self->split_keys($keys);
    }
    for my $key (@$keys) {
        #(my $pat = $key) =~ s/\%/\%\%/g;
        $self->limit(
            column => 'content', value => "%$key%", operator => 'LIKE'
        );
    }
    $self->order_by(column => 'session_offset', order => 'DES');
}

1;

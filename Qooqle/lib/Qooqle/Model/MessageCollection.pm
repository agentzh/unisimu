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
    my $sessions = $self->new_alias('sessions');
    $self->join(
        column1 => 'msg_session',
        alias2  => $sessions, column2 => 'id');
    for my $key (@$keys) {
        #(my $pat = $key) =~ s/\%/\%\%/g;
        $self->limit(
            column => 'content', value => "%$key%", operator => 'LIKE',
            entry_aggregator => 'AND',
        );
    }
    $self->order_by(
        {alias => $sessions, column => 'begin_time', order => 'DESC'},
        {column => 'session_offset', order => 'ASC'},
    );
}

1;

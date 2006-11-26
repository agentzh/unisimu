#!/usr/bin/env perl
use warnings;
use strict;

=head1 DESCRIPTION

A basic test harness for the Session model.

=cut

use Jifty::Test tests => 17;

# Make sure we can load the model
use_ok('Qooqle::Model::Session');

# Grab a system user
my $system_user = Qooqle::CurrentUser->superuser;
ok($system_user, "Found a system user");

# Try testing a create
my $o = Qooqle::Model::Session->new(current_user => $system_user);
my ($id) = $o->create(
    begin_time => '2006-09-20 5:00',
    end_time => '2006-10-20 4:21',
    message_count => 3,
);
ok($id, "Session create returned success");
ok($o->id, "New Session has valid id set");
is($o->id, $id, "Create returned the right id");
is($o->begin_time, '2006-09-20 5:00', 'begin time ok');
is($o->end_time, '2006-10-20 4:21', 'end time ok');
is($o->message_count, 3, 'message count ok');

# And another
$o->create(
    begin_time => '1985-12-09 18:30',
    end_time   => '1992-10-24 06:21',
    message_count => 0,
);
ok($o->id, "Session create returned another value");
isnt($o->id, $id, "And it is different from the previous one");
is($o->begin_time, '1985-12-09 18:30', 'begin time ok');
is($o->end_time, '1992-10-24 06:21', 'end time ok');
is($o->message_count, 0, 'message count ok');

# Searches in general
my $collection =  Qooqle::Model::SessionCollection->new(current_user => $system_user);
$collection->unlimit;
is($collection->count, 2, "Finds two records");

# Searches in specific
$collection->limit(column => 'id', value => $o->id);
is($collection->count, 1, "Finds one record with specific id");

# Delete one of them
$o->delete;
$collection->redo_search;
is($collection->count, 0, "Deleted row is gone");

# And the other one is still there
$collection->unlimit;
is($collection->count, 1, "Still one left");


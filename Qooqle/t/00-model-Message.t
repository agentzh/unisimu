#!/usr/bin/env perl
use warnings;
use strict;

=head1 DESCRIPTION

A basic test harness for the Message model.

=cut

use Jifty::Test tests => 24;

# Make sure we can load the model
use_ok('Qooqle::Model::Message');

# Grab a system user
my $system_user = Qooqle::CurrentUser->superuser;
ok($system_user, "Found a system user");

my $user = Qooqle::Model::QQUser->new(current_user => $system_user);

$user->create(
    qq_number => '279005114',
    realname => 'ÕÂÒà´º',
    nickname  => 'Agent2002',
    gender    => 'M'
);

$user->create(
    qq_number => '11854905',
    realname => 'ñÒöÎêÀ',
    gender    => 'F',
);

$user->create(
    qq_number => '321352111',
    gender    => 'F',
);

my $session = Qooqle::Model::Session->new(current_user => $system_user);
$session->create(
    begin_time    => '2005-10-22 2:30',
    end_time      => '2005-10-22 5:20',
    message_count => '3',
);
ok $session->id, 'session created';

# Try testing a create
my $msg = Qooqle::Model::Message->new(current_user => $system_user);

my $content = <<'_EOC_';
How's jifty going?
Oh, well...
_EOC_

my ($first_id) = $msg->create(
    receiver => '11854905',
    sender   => '321352111',
    sent     => '2006-11-23 12:03:01',
    content  => $content,
    session_id  => $session->id,
    session_offset   => 0,
);

ok($first_id, "Message create returned success");
ok($msg->id, "New Message has valid id set");
is($msg->id, $first_id, "Create returned the right id");
is($msg->receiver->qq_number, '11854905', 'receiver reads ok');
is($msg->sender->qq_number, '321352111', 'sender reads ok');
is($msg->sent, '2006-11-23 12:03:01');

# create another message
$content = <<'_EOC_';
Hi, there~
I really love Jifty!
And...I'm rewriting Qooqle using Jifty!
_EOC_

my ($second_id) = $msg->create(
    receiver => '11854905',
    sender => '279005114',
    sent => '1163924434',
    content => $content,
    session_id => $session->id,
    session_offset => 3,
);
ok($second_id, "Successfully created the second Message");
is $msg->session->id, $session->id, 'session ok';
is $msg->session_offset, 3, 'session session_offset ok';
is $msg->content, $content, 'content ok';
is $msg->sent, '2006-11-19 08:20:34', 'epoch conversion worked';

# Searches in general
my $collection = Qooqle::Model::MessageCollection->new(current_user => $system_user);
$collection->unlimit;
is($collection->count, 2, "Finds two records");

$collection->limit(
    column => 'session_offset',
    operator => '<',
    value => 2
);
is($collection->count, 1, "Finds one record");
is $collection->last->id, $first_id, 'found the right group';

$collection->unlimit;

# Searches in specific
$collection->limit(column => 'id', value => $msg->id);
is($collection->count, 1, "Finds one record with specific id");

$collection->unlimit;

# Searches in specific
$collection->limit(column => 'content', operator => 'LIKE', value => '%love%');
is($collection->count, 1, "Finds one record with 'love'");
my $entry = $collection->next;
like $entry->content, qr/love/, 'like "love"';

$collection->unlimit;

# Searches in specific
$collection->limit(column => 'content', operator => 'NOT LIKE', value => '%love%');
is($collection->count, 1, "Finds one record without 'love'");
$entry = $collection->next;
unlike $entry->content, qr/love/, 'like "love"';

$collection->unlimit;

# Searches in specific
$collection->limit(column => 'content', operator => 'LIKE', value => '%love%');
$collection->limit(column => 'content', operator => 'LIKE', value => '%well%');
is($collection->count, 2, "Finds two records with 'love' OR 'well'");

$collection->unlimit;

# Searches in specific
$collection->limit(column => 'content', operator => 'LIKE', value => '%love%');
$collection->limit(
    entry_aggregator => 'and',
    column => 'content', operator => 'LIKE', value => '%well%'
);
is($collection->count, 0, "Finds no records with 'love' AND 'well'");

#!/usr/bin/env perl
use warnings;
use strict;
use encoding 'GB2312';

=head1 DESCRIPTION

A basic test harness for the QQUser model.

=cut

use Jifty::Test tests => 25;

# Make sure we can load the model
use_ok('Qooqle::Model::QQUser');

# Grab a system user
my $system_user = Qooqle::CurrentUser->superuser;
ok($system_user, "Found a system user");

# create a user named agent
my $user = Qooqle::Model::QQUser->new(current_user => $system_user);
my ($agent_id) = $user->create(
    qq_number => '279005114',
    realname => 'ÕÂÒà´º',
    nickname => 'Agent2002',
    gender => 'M'
);
ok($agent_id, 'user agent created.');
is $user->id, $agent_id, 'id ok';
is $user->qq_number, '279005114', "agent's qq_number reads ok";
is $user->realname, 'ÕÂÒà´º', "agent's realname reads ok";
is $user->nickname, 'Agent2002', "agent's nickname reads ok";
is $user->gender, 'M', "agent's gender reads ok";

my ($cherry_id) = $user->create(
    qq_number => '11854905',
    realname => 'ñÒöÎêÀ',
    gender => 'F',
);
ok($cherry_id, 'user cherry created.');
is $user->qq_number, '11854905', "cherry's qq_number reads ok";
is $user->realname, 'ñÒöÎêÀ', "cherry's realname reads ok";
ok !$user->nickname, "cherry's nickname is undef";
is $user->gender, 'F', "cherry's gender reads ok";

my ($clover_id) = $user->create(
    qq_number => '321352111',
    gender => 'F',
);
ok($clover_id, 'user cherry created.');
is $user->qq_number, '321352111', "clover's qq_number reads ok";
ok !$user->realname, "clover's realname reads ok";
ok !$user->nickname, "clover's nickname is undef";
is $user->gender, 'F', "clover's gender reads ok";

# Searches in general
my $collection =  Qooqle::Model::QQUserCollection->new(current_user => $system_user);
$collection->unlimit;
is($collection->count, 3, "Finds two records");

# Searches in specific
$collection->limit(column => 'id', value => $cherry_id);
is($collection->count, 1, "Finds one record with specific id");
is $collection->first->qq_number, '11854905', 'cherry found via id';

$collection->unlimit;

$collection->limit(column => 'gender', value => 'M');
is($collection->count, 1, "Finds one record with specific gender");
is $collection->next->id, $agent_id, 'agent found via gender';

# Delete one of them
$user->load($agent_id);
$user->delete;
$collection->redo_search;
is($collection->count, 0, "Deleted row is gone");

# And the other two are still there
$collection->unlimit;
is($collection->count, 2, "Still two left");

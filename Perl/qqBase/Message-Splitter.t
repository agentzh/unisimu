use strict;
use warnings;

use Test::More tests=>19;
BEGIN { use_ok('Message::Splitter'); }

my $obj = Message::Splitter->new(5);
ok $obj->should_split;

$obj->add(a=>5);
ok !$obj->should_split;

$obj->add(a=>8);
ok !$obj->should_split;

$obj->add(a=>10);
ok !$obj->should_split;

$obj->add(a=>20);
ok $obj->should_split;

$obj->add(a=>25);
ok $obj->should_split;

$obj->add(a=>26);
ok !$obj->should_split;

$obj->add(a=>26);
ok !$obj->should_split;

$obj->add(a=>20);
ok !$obj->should_split;

###

$obj = Message::Splitter->new(5);
ok $obj->should_split;

$obj->add(b=>5);
ok !$obj->should_split;

$obj->add(b=>8);
ok !$obj->should_split;

$obj->add(b=>10);
ok !$obj->should_split;

$obj->add(b=>20);
ok $obj->should_split;

$obj->add(b=>25);
ok $obj->should_split;

$obj->add(b=>26);
ok !$obj->should_split;

$obj->add(b=>26);
ok !$obj->should_split;

$obj->add(b=>20);
ok !$obj->should_split;

use strict;
use warnings;

use Test::More tests=>77;
BEGIN { use_ok('Qooqle::Message::Splitter'); }

my $obj = Qooqle::Message::Splitter->new(5);
ok $obj->should_split;

$obj->add(a=>5);
ok not $obj->should_split;

$obj->add(a=>8);
ok not $obj->should_split;

$obj->add(a=>10);
ok not $obj->should_split;

$obj->add(a=>20);
ok $obj->should_split;

$obj->add(a=>25);
ok $obj->should_split;

$obj->add(a=>26);
ok not $obj->should_split;

$obj->add(a=>26);
ok not $obj->should_split;

$obj->add(a=>20);
ok not $obj->should_split;

###

$obj = Qooqle::Message::Splitter->new(5);
ok $obj->should_split;

$obj->add(b=>5);
ok not $obj->should_split;

$obj->add(b=>8);
ok not $obj->should_split;

$obj->add(b=>10);
ok not $obj->should_split;

$obj->add(b=>20);
ok $obj->should_split;

$obj->add(b=>25);
ok $obj->should_split;

$obj->add(b=>26);
ok not $obj->should_split;

$obj->add(b=>26);
ok not $obj->should_split;

$obj->add(b=>20);
ok not $obj->should_split;

###

$obj = Qooqle::Message::Splitter->new(6);
ok $obj->should_split;

$obj->add(b=>2);
ok not $obj->should_split;

$obj->add(a=>5+50);
ok not $obj->should_split;

$obj->add(b=>7);
ok not $obj->should_split;

$obj->add(b=>10);
ok not $obj->should_split;

$obj->add(a=>13+50);
ok not $obj->should_split;

$obj->add(b=>17);
ok $obj->should_split;

###

$obj = Qooqle::Message::Splitter->new(5);
ok $obj->should_split;

$obj->add(b=>2+50);
ok not $obj->should_split;

$obj->add(a=>5);
ok not $obj->should_split;

$obj->add(b=>7+50);
ok $obj->should_split, 'split!';

$obj->add(b=>10+50);
ok not $obj->should_split;

$obj->add(a=>15);
ok not $obj->should_split;

$obj->add(b=>15+50);
ok $obj->should_split;

###

$obj = Qooqle::Message::Splitter->new(6);
ok $obj->should_split;

$obj->add(a=>2);
ok not $obj->should_split;

$obj->add(b=>5+50);
ok not $obj->should_split;

$obj->add(a=>7);
ok not $obj->should_split;

$obj->add(a=>10);
ok not $obj->should_split;

$obj->add(b=>13+50);
ok not $obj->should_split;

$obj->add(a=>17);
ok $obj->should_split;

###

$obj = Qooqle::Message::Splitter->new(5);
ok $obj->should_split;

$obj->add(a=>2+20);
ok not $obj->should_split;

$obj->add(b=>5);
ok not $obj->should_split;

$obj->add(a=>7+20);
ok $obj->should_split, 'split!';

$obj->add(a=>10+20);
ok not $obj->should_split;

$obj->add(b=>15);
ok not $obj->should_split;

$obj->add(a=>15+20);
ok $obj->should_split;

###

$obj = Qooqle::Message::Splitter->new(6);
ok $obj->should_split;

$obj->add(b=>2);
ok not $obj->should_split;

$obj->add(a=>5);
ok not $obj->should_split;

$obj->add(b=>7);
ok not $obj->should_split;

$obj->add(b=>10);
ok not $obj->should_split;

$obj->add(a=>20);
ok $obj->should_split;

$obj->add(b=>17);
ok not $obj->should_split;

###

$obj = Qooqle::Message::Splitter->new(5);
ok $obj->should_split;

$obj->add(b=>0);
ok not $obj->should_split;

$obj->add(a=>25);
ok not $obj->should_split;

$obj->add(b=>3);
ok not $obj->should_split;

$obj->add(a=>28);
ok not $obj->should_split;

$obj->add(b=>5);
ok not $obj->should_split;

$obj->add(a=>31);
ok not $obj->should_split;

$obj->add(b=>6);
ok not $obj->should_split;

$obj->add(a=>35);
ok not $obj->should_split;

###

$obj = Qooqle::Message::Splitter->new(5);
ok $obj->should_split;

$obj->add(a=>0);
ok not $obj->should_split;

$obj->add(b=>25);
ok not $obj->should_split;

$obj->add(a=>3);
ok not $obj->should_split;

$obj->add(b=>28);
ok not $obj->should_split;

$obj->add(a=>5);
ok not $obj->should_split;

$obj->add(b=>31);
ok not $obj->should_split;

$obj->add(a=>6);
ok not $obj->should_split;

$obj->add(b=>35);
ok not $obj->should_split;

###

$obj = Qooqle::Message::Splitter->new(20 * 60);
ok $obj->should_split;

$obj->add(a=>67171);
ok not $obj->should_split;

$obj->add(b=>74012);
ok not $obj->should_split;

$obj->add(a=>74440);
ok not $obj->should_split;

$obj->add(b=>74052);
ok not $obj->should_split;

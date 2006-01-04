use strict;
use Test::More tests => 3;
BEGIN { 
    use_ok('GetPoint::App')
};

my $app = GetPoint::App->new;
ok $app;
isa_ok $app, 'GetPoint::App';

use strict;
use inc::Module::Install;

my $gvmake = "script/gvmake";

name            ('GD-Animation');
license         ('perl');
all_from        ('lib/GD/Animation.pm');

requires        ('Perl6::Attributes');

build_requires  ('Test::More');

&WriteAll;

use strict;
use inc::Module::Install;

my $gvmake = "script/gvmake";

name            ('Pod-Extend');
version_from    ('lib/Pod/Extend.pm');
abstract_from   ('lib/Pod/Extend.pm');
author          ('Agent Zhang <agent2002@126.com>');
license         ('perl');

install_script  ('script/podx');

requires        ('DBI');
requires        ('DBD::SQLite');
requires        ('Text::Table');
requires        ('Perl6::Slurp');
requires        ('Scalar::Util');
requires        ('PerlMaple' => '0.02');

build_requires  ('Test::More');
build_requires  ('File::Compare');

&WriteAll;

use strict;
use warnings;
use inc::Module::Install;

name            ('GetPoint');
version_from    ('lib/GetPoint/App.pm');
abstract_from   ('lib/GetPoint/App.pm');
author          ('Agent Zhang <agent2002@126.com>');
license         ('perl');

install_script  ('script/getpoint');

requires        ('Carp');
requires        ('Wx');
requires        ('Wx::Perl::Imagick');
requires        ('List::MoreUtils');

build_requires  ('Test::More');
build_requires  ('Test::Deep');

&WriteAll();

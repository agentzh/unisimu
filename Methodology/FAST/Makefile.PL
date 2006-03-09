use strict;
use inc::Module::Install;

name            ('FAST');
all_from        ('lib/FAST.pm');
license         ('perl');

install_script  ('script/fplot.pl');
#install_script  ('script/fast.pl');

#requires        ('Data::Dumper::Simple');
requires        ('GraphViz');
requires        ('Clone');

build_requires  ('Test::Simple');
build_requires  ('Test::MockObject');
build_requires  ('Test::Differences');

#makemaker_args  (
#    test => { TESTS => "t/*.t t/*/*.t t/*/*/*.t" },
#);

auto_install;

&WriteAll;

use strict;
use inc::Module::Install;

name            ('FAST');
all_from        ('lib/FAST.pm');
license         ('perl');

install_script  ('script/fplot.pl');
#install_script  ('script/fast.pl');

#requires        ('Data::Dumper::Simple');

build_requires  ('Test::Simple');

#makemaker_args  (
#    test => { TESTS => "t/*.t t/*/*.t t/*/*/*.t" },
#);

&WriteAll;

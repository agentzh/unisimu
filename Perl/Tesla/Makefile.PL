use strict;
use inc::Module::Install;

name            ('Tesla');
author          ('Agent Zhang');
license         ('perl');
requires        (perl => 5.006001);
all_from        ('lib/Tesla.pm');

install_script  ('script/plotsig.pl');
install_script  ('script/tslc.pl');

#requires        ('Data::Dumper::Simple');

build_requires  ('Test::Simple');
build_requires  ('Data::Dumper');
build_requires  ('Carp');

makemaker_args  (
    test => { TESTS => "t/*.t" },
);

&auto_install;

&WriteAll;

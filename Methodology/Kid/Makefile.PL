use strict;
use inc::Module::Install;

perl_version    ('5.006001');
name            ('Kid');
license         ('perl');
all_from        ('lib/Kid.pm');

for my $ext (qw[ kid pl xml mpl mm mms ]) {
    install_script  ("script/kid2$ext.pl");
}

#requires        ('Data::Dumper::Simple');
requires  ('Clone');
requires  ('Language::AttributeGrammar');
requires  ('Parse::RecDescent');
requires  ('PerlMaple');

build_requires  ('Test::Base');

makemaker_args  (
    test => { TESTS => "t/*.t t/*/*.t" },
);

&WriteAll;

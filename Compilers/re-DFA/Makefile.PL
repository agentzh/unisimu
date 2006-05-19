use strict;
use inc::Module::Install;

perl_version    ('5.006001');
name            ('re-DFA');
license         ('perl');
all_from        ('lib/re/DFA.pm');

for my $ext (qw[ nfa dfa xml re ]) {
    install_script  ("script/re2$ext.pl");
}

#requires        ('Data::Dumper::Simple');
requires  ('Set::Scalar');
requires  ('Language::AttributeGrammar');
requires  ('Parse::RecDescent');
requires  ('List::Util');
requires  ('List::MoreUtils');
requires  ('GraphViz');
requires  ('Carp');
requires  ('Clone');

build_requires  ('IPC::Run3');
build_requires  ('Test::More');
build_requires  ('Test::Base');
build_requires  ('File::Compare');

makemaker_args  (
    test => { TESTS => "t/*.t t/*/*.t" },
);

&WriteAll;

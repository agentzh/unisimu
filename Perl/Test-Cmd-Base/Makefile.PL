use strict;
use inc::Module::Install;

all_from    ('lib/Test/Cmd/Base.pm');
license         ('perl');

requires  ('Test::Base');
requires  ('IPC::Run3');
requires  ('File::Temp');
requires  ('File::Spec');
requires  ('Cwd');
requires  ('Text::Balanced');
requiers  ('FindBin');

#makemaker_args  (
#    test => { TESTS => "t/*.t t/*/*.t t/*/*/*.t" },
#);

&WriteAll;

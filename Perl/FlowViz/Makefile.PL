use strict;
use inc::Module::Install;
#my $plmake = "script/plmake";

name            ('Flowchart');
all_from        ('lib/GraphViz/Flowchart.pm');
license         ('perl');

#install_script  ($plmake);

build_requires  ('Parse::RecDescent');

#build_requires  ('Test::Simple');
build_requires  ('Test::Base');
#build_requires  ('IPC::Run3');
#build_requires  ('File::Temp');
build_requires  ('File::Compare');
#build_requires  ('Cwd');
#build_requires  ('Text::Balanced');
#build_requiers  ('FindBin');
#build_requires  ('Time::HiRes');

makemaker_args  (
    test => { TESTS => "t/Flowchart_Asm/*.t t/Flowchart_C/*.t"},
);

&WriteAll;



use strict;
use inc::Module::Install;

name            ('UML-Class-Simple');
license         ('perl');
all_from        ('lib/UML/Class/Simple.pm');

install_script  ('script/umlclass.pl');

requires        ('File::Slurp');
requires        ('YAML::Syck');
requires        ('Carp');
requires        ('File::Spec');
requires        ('PPI'              => '1.118');
requires        ('Class::Inspector' => '1.16');
requires        ('IPC::Run3'        => '0.036');
requires        ('List::MoreUtils'  => '0.21');
requires        ('Template'         => '2.14');

build_requires  ('Test::More'       => '0.64');

auto_install();
WriteAll();

END {
    my $s = `dot -V 2>&1`;
    if (!defined $s) {
        die "error: The `dot` utility of your Graphviz not found in your system: $s\n";
    }
    if ($s =~ /(?x) version \s+ (\d+ \. \d+) /i) {
        my $ver = $1;
        if ($ver < 1.12) {
            die "error: You Graphviz ($&) is too old to support ",
                "the HTML label feature. Please download a more ",
                "recent version from http://www.graphviz.org/\n";
        } else {
            warn "info: Your Graphviz installation looks OK: $&\n";
        }
    } else {
        warn "Warning: the version of Graphviz dot is unknown: $s\n";
    }
}

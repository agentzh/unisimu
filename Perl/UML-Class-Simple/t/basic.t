use strict;
use warnings;

use Test::More tests => 29;
use UML::Class::Simple;
#use Data::Dumper::Simple;

use UML::Class::Simple;

my (@classes, $painter);

@classes = classes_from_runtime("PPI", qr/^PPI::/);
ok @classes > 5, 'a lot of PPI classes found';

$painter = UML::Class::Simple->new(\@classes);
ok $painter, 'painter obj created';
isa_ok $painter, 'UML::Class::Simple';

is $painter->node_color, '#f1e1f4', "node_color's default value ok";

#warn Dumper($painter->as_dom);

my $imgfile = 't/ppi.png';
unlink $imgfile if -f $imgfile;
$painter->as_png($imgfile);
ok -f $imgfile, "image '$imgfile' generated";
ok((-s $imgfile) > 1000, 'image is not empty');

@classes = classes_from_runtime("PPI", qr/^PPI::Document$/);
is scalar(@classes), 1, 'PPI Document found (1)';
is $classes[0], 'PPI::Document', 'PPI Document found (2)';

# produce a class diagram for your CPAN module on the disk

@classes = classes_from_files(['lib/UML/Class/Simple.pm', 'lib/UML/Class/Simple.pm']);
is join(' ', @classes), 'UML::Class::Simple UML::Class::Simple', 'classes found';
$painter = UML::Class::Simple->new(\@classes);

# we can explicitly specify the image size
ok $painter->size(5, 3.6), 'setting size ok'; # in inches
my ($w, $h) = $painter->size;
is $w, 5, 'width ok';
is $h, '3.6', 'height ok';

ok ! $painter->size('foo', 'bar'), 'setting size with invalid values';
is $w, 5, 'width not changed';
is $h, '3.6', 'height not changed either';

# ...and change the default title background color:
$painter->node_color('#ffeeff'); # defaults to '#f1e1f4'
is $painter->node_color, '#ffeeff', "node_color's default value changed";

my $dom = $painter->as_dom;
ok $dom, '$dom ok';
ok ref $dom, '$dom is a ref';
is_deeply $dom, {
    classes => [
        { name       => 'UML::Class::Simple',
          methods    => [qw(
                _as_image _build_dom _gen_paths _load_file
                _runtime_packages
                any as_dom as_dot as_gif as_png carp
                classes_from_files
                classes_from_runtime new node_color public_only
                run3 set_dom set_dot size
                        )],
          properties => [],
          subclasses => [],
        }
    ],
}, '$dom structure ok';

# only show public methods and properties
ok ! $painter->public_only, 'public_only defaults to false';
$painter->public_only(1);
ok $painter->public_only, 'public_only changed to true';

$dom = $painter->as_dom;
is_deeply $dom, {
    classes => [
        { name       => 'UML::Class::Simple',
          methods    => [qw(
                any as_dom as_dot as_gif as_png carp classes_from_files
                classes_from_runtime new node_color public_only
                run3 set_dom set_dot size
                        )],
          properties => [],
          subclasses => [],
        }
    ],
}, '$dom structure ok';

my $dot = $painter->as_dot;
like $dot, qr/^digraph uml_class_diagram \{/, 'dot looks ok';
like $dot, qr/size="5,3.6";/, 'size set ok';
like $dot, qr/="\#ffeeff"/, 'color set ok';

my $dotfile = 't/me.dot';
unlink $dotfile if -f $dotfile;
$painter->as_dot($dotfile);
ok -f $dotfile, "dot file '$dotfile' generated";
ok -s $dotfile, "dot file '$dotfile' is not empty";

my $bin = $painter->as_png;
ok length($bin) > 1000, 'binary PNG data returned';

$bin = $painter->as_gif;
ok length($bin) > 1000, 'binary GIF data returned';

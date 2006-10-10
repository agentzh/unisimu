use strict;
use warnings;

my $class_path = shift;
my $class;
if ($class_path =~ /\.(\w+)$/) {
    $class = $1;
} else {
    die "syntax error: $class_path";
}

(my $class_path2 = $class_path) =~ s/\./::/g;

my $str = <<"_EOC_";
package $class;

use strict;
use warnings;

#use Java::Swing::ActionListener;

use Inline Java      => 'STUDY',
           AUTOSTUDY => 1,
           STUDY     => ['$class_path'];

sub new {
    shift;
    ${class}::$class_path2->new(@_);
}

1;
_EOC_

my $pm = "$class.pm";
open my $out, "> $pm" or
    die "Can't open $pm for writing: $!";
print $out $str;
close $out;
warn "$pm generated.\n";
eval {
    require "$pm";
};
die $@ if $@;

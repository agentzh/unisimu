use strict;
use warnings;

use File::Spec;
use List::MoreUtils 'uniq';
use Getopt::Std;
use JavaScript::SpiderMonkey;
use JSON::Syck;

my %opts;
getopts("I:", \%opts);

my @JS_INC;
@JS_INC = split(/\s*;\*/, $ENV{'JS_INC'}) if $ENV{'JS_INC'};
unshift @JS_INC, $opts{I} if $opts{I};
push @JS_INC, '.';
@JS_INC = uniq @JS_INC;

my $infile = shift or die "No JavaScript file specified.\n";

open my $in, $infile or
    die "Can't open $infile for reading: $!\n";
my $jssrc;
{
    local $/; 
    $jssrc = <$in>;
}
close $in;

my $js = JavaScript::SpiderMonkey->new();
$js->init();
$js->function_set(
    "require",
    sub {
        my $path = $_[0];
        load_js($path);
    }
);
$js->function_set(
    "use",
    sub {
        my $name = shift;
        (my $path = $name) =~ s/\./\//g;
        $path .= ".js";
        load_js($path);
    }
);
$js->function_set("print", sub { print @_; });
$js->function_set("say", sub { print @_, "\n"; });
$js->function_set("alert", sub { warn @_; });
$js->function_set("die", sub { die @_, "\n"; });

my $s = JSON::Syck::Dump(\@ARGV);
#warn $s;
$js->eval("arguments = $s;\n") or die $@;
$s = JSON::Syck::Dump(\@JS_INC);
$js->eval("JS_INC = $s;\n") or die $@;

JavaScript::SpiderMonkey::JS_EvaluateScript(
    $js->{context}, $js->{global_object},
    $jssrc, length($jssrc), $infile, 1) or warn "$infile: $@\n";

$js->destroy();

sub load_js {
    my ($fname) = @_;
    my $path;
    for my $dir (@JS_INC) {
        my $temp = File::Spec->catfile($dir, $fname);
        if (-f $temp) { $path = $temp; last; }
    }
    if (!defined $path) {
        die "Fail to find JS module $fname (JS_INC contains ",
            join(' ', map { "\"$_\"" } @JS_INC), ")\n";
    }
    open my $in, $path or
        die "Fail to load JS module $path: $!\n";
    my $src;
    {
        local $/;
        $src = <$in>;
    }
    close $in;
    #warn $src;
    if (not JavaScript::SpiderMonkey::JS_EvaluateScript(
        $js->{context}, $js->{global_object},
        $src, length($src), $path, 0
    )) {
        my $js = JavaScript::SpiderMonkey->new();
        $js->init();
        $js->function_set(
            "require",
            sub {
                my $path = $_[0];
                load_js($path);
            }
        );
        $js->function_set(
            "use",
            sub {
                my $name = shift;
                (my $path = $name) =~ s/\./\//g;
                $path .= ".js";
                load_js($path);
            }
        );
        JavaScript::SpiderMonkey::JS_EvaluateScript(
                $js->{context}, $js->{global_object},
                $src, length($src), $path, 1);
        warn "$path: $@";
    }
}

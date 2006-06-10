use strict;
use warnings;
use JavaScript::SpiderMonkey;
use JSON::Syck;

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
$js->function_set("require", sub { print "require @_\n"; });
$js->function_set("use", sub { print "use @_\n"; });
$js->function_set("print", sub { print @_; });
$js->function_set("say", sub { print @_, "\n"; });
$js->function_set("alert", sub { warn @_, "\n"; });
$js->function_set(
    "dump", 
    sub { 
        if (@_ > 1) {
            print JSON::Syck::Dump(\@_);
        } else {
            print JSON::Syck::Dump($_[0]);
        }
        return "'abc'";
    }
);

my $s = JSON::Syck::Dump(\@ARGV);
#warn $s;
$js->eval("args = $s;\n") or die $@;

if (not $js->eval($jssrc)) {
    die "JavaScript error: $@\n";
}

$js->destroy();

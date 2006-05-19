# scripts.t
# test script/*.pl

use strict;
use warnings;

use Test::More tests => 14;
use IPC::Run3;
use Data::Dumper::Simple;

our $Regex = '(a|b)*';

sub test {
    my $script = shift;
    my $expected = shift;
    my @outfiles = @_;

    for my $file (@outfiles) {
        unlink $file if -f $file;
    }

    my $cmd = [$^X, "-Ilib", "script/$script.pl", $Regex];
    #warn Dumper($cmd);
    my $stdout;
    run3 $cmd, \undef, \$stdout;
    ok defined $stdout, "$script - start ok";
    #warn "$buffer";
    $stdout =~ s/[\s\n]+$//gs;
    if (ref $expected) {
        like($stdout, $expected, "$script - output ok");
    } else {
        is($stdout, $expected, "$script - output ok");
    }

    for my $file (@outfiles) {
        ok -f $file, "$script - outfile $file creation ok";
        ok -s $file, "$script - outfile $file size ok";
        unlink $file if -f $file;
    }
}

test( 're2re', $Regex );
test( 're2xml', qr/<expression>/ );
test( 're2nfa', "NFA.png generated.", 'NFA.png' );
test( 're2dfa', "DFA.png generated.\nDFA.min.png generated.", 'DFA.png', 'DFA.min.png' );

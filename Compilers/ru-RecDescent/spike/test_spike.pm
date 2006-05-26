# test_spike.pm
# testbed for spike.pl

package test_spike;

use Test::Base -Base;
use File::Temp qw/ tempfile /;
#use Data::Dumper::Simple;

our @EXPORT = qw(run_tests);

my $pmfile;
my @pmfiles;

mkdir 'tmp' if !-d 'tmp';

my $counter = 0;
my $parser;

filters {
    ast => 'eval',
};

sub run_tests() {
    for my $block (blocks()) {
        run_test($block);
    }
}

sub run_test($) {
    my $block = shift;
    my $gm = $block->grammar;
    my $input = $block->input;
    my $expected_ast = $block->ast;
    my $name = $block->name;

    if (defined $gm) {
        my ($fh, $gmfile) =
            tempfile('gm_XXXXXX', SUFFIX => '.grammar', UNLINK => 1, DIR => 'tmp');
        #warn "Grammar File: $gmfile";
        print $fh $gm;
        close $fh;
        my $class = 'Parser' . (++$counter);
        is system($^X, 'spike.pl', '-m', "-n $class", $gmfile), 0, "$name - spike.pl";
        ($pmfile = $gmfile) =~ s/\.grammar$/.pm/;
        ok -f $pmfile, "$name - $pmfile ok";
        ok require $pmfile, "$name - load module $pmfile ok";
        $parser = $class->new;
    }

    #$::RD_TRACE = 1;
    my $ast = $parser->parse($input);
    #warn Dumper($ast);
    is_deeply $ast, $expected_ast, "$name - parse tree ok";
    push @pmfiles, $pmfile;
}

END {
    for my $file (@pmfiles) {
        unlink $pmfile;
    }
}

1;

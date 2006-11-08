package t::VRG;

use Test::Base -Base;
use IPC::Run3;
use File::Slurp;

our @EXPORT = qw(run_tests);

my $count = 0;

my $xclips = "$^X xclips.pl";

#no_diff;

sub run_tests () {
    for my $block (blocks()) {
        #warn "HERE!";
        run_test($block);
    }
}

sub run_test () {
    my $block = shift;
    my $name = $block->name;
    my $id = sprintf("%03d", ++$count);

    my ($stdout, $stderr);

    my $vrg_src = $block->vrg;
    my $vrg_file = "$id.vrg";
    my $xclp_file = "$id.xclp";

    unlink $xclp_file if -f $xclp_file;

    write_file($vrg_file, $vrg_src);
    ok run3(
        [$^X, 'vrgs.pl', $vrg_file],
        \undef,
        \$stdout,
        \$stderr,
    ), 'invoking vrgs.pl ok';
    ok !$stderr, 'no stderr';
    warn $stderr if $stderr;

    my $xclp_src = read_file($xclp_file) if -f $xclp_file;
    is $xclp_src, $block->xclp, "$name - XClips source ok";

    #write_file($xclp_file, $block->xclp);
    ok system(split(/\s+/, $xclips), $xclp_file) == 0, "$name - invoking $xclips ok";

    my $clp_file = "$id.clp";
    ok run3(
            [$^X, 'vrg-run.pl', $clp_file],
            \undef,
            \$stdout,
            \$stderr,
        ),
        "$name - vrg-run.pl ok";
    warn $stderr if $stderr;
    #warn $stdout;
    my ($vectorize, $eval, $final) = ($stdout =~ /(.*)---\n(.*)---\n(.*)/s);
    my $got = sort_list($vectorize);
    #warn "!!!!", $vectorize, "!!!!", $block->vectorize, "!!!!";
    my $expected = sort_list($block->vectorize);
    is $got, $expected, "$name - vectorization ok";
    if ($block->eval) {
        my @rels = split /\n/, $block->eval;
        for my $rel (@rels) {
            my $pat = quotemeta($rel);
            like $eval, qr/\b$pat\n/ms, "$name -- vectorizie -- $rel appeared";
        }
    }
    if ($block->final) {
        my @rels = split /\n/, $block->final;
        for my $rel (@rels) {
            my $pat = quotemeta($rel);
            like $final, qr/\b$pat\n/ms, "$name -- final -- $rel appeard";
        }
    }
};

sub sort_list () {
    my $s = shift;
    if (!defined $s) { my @a = caller; warn "~~~~ line $a[2]\n"; }
    my @ln = split /\n/, $s;
    join( "\n", sort @ln )."\n";
}

1;

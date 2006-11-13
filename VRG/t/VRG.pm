package t::VRG;

use Test::Base -Base;
use IPC::Run3;
use File::Slurp;

our @EXPORT = qw(run_tests);

my $count = 0;

my $xclips = "$^X -Ilib script/xclips.pl -c -I knowledge";

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
    run3(
        [$^X, 'script/vrgs.pl', $vrg_file],
        \undef,
        \$stdout,
        \$stderr,
    );
    is $?, 0, 'invoking vrgs.pl ok';
    ok !$stderr, 'no stderr';
    warn $stderr if $stderr;

    if ($block->xclp) {
        my $xclp_src = read_file($xclp_file) if -f $xclp_file;
        is $xclp_src, $block->xclp, "$name - XClips source ok";
    }

    #write_file($xclp_file, $block->xclp);
    ok system(split(/\s+/, $xclips), $xclp_file) == 0, "$name - invoking $xclips ok";

    my $clp_file = "$id.clp";
    ok run3(
            [$^X, 'script/vrg-run.pl', '-t', $clp_file],
            \undef,
            \$stdout,
            \$stderr,
        ),
        "$name - vrg-run.pl ok";
    warn $stderr if $stderr;
    #warn $stdout;

    $stdout =~ s/^(.*?\n)\n//s;
    my $ans_got = $1;

    my ($vectorize, $eval, $final) = ($stdout =~ /(.*)---\n(.*)---\n(.*)/s);
    if (defined $block->vectorize) {
        my $got = sort_list($vectorize);
        #warn "!!!!", $vectorize, "!!!!", $block->vectorize, "!!!!";
        my $expected = sort_list($block->vectorize);
        is $got, $expected, "$name - vectorize ok";
    }
    if (defined $block->eval) {
        my @rels = split /\n/, $block->eval;
        for my $rel (@rels) {
            my $pat = quotemeta($rel);
            like $eval, qr/\b$pat\n/ms, "$name -- vector-eval ok -- $rel appeared";
        }
    }
    if (defined $block->antivec) {
        my @rels = split /\n/, $block->antivec;
        for my $rel (@rels) {
            my $pat = quotemeta($rel);
            like $final, qr/\b$pat\n/ms, "$name -- anti-vectorize ok -- $rel appeard";
        }
    }

    my $ans = $block->ans;
    if (defined $ans) {
        is $ans_got, $ans, "$name - ans ok";
    } else {
        is $ans_got, "Yes.\n", "$name - ans ok";
    }
};

sub sort_list () {
    my $s = shift;
    if (!defined $s) { my @a = caller; warn "~~~~ line $a[2]\n"; }
    my @ln = split /\n/, $s;
    join( "\n", sort @ln )."\n";
}

1;

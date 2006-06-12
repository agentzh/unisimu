# LL1.t
# Test the ru-LL1 compiler LL1.pl

use strict;
use warnings;

use File::Temp qw/ tempfile /;
use Test::Base;
use IPC::Run3;

plan tests => 3 * blocks() + 4;

my $plfile;
my @plfiles;

mkdir 'tmp' if !-d 'tmp';

filters {
    opts => ['chomp'],
};

run {
    my $block = shift;
    my $opts = $block->opts || '';
    my $gm = $block->grammar;
    my $input = $block->input;
    my $name = $block->name;

    if (defined $gm) {
        my ($fh, $gmfile) =
            tempfile('gm_XXXXXX', SUFFIX => '.grammar', UNLINK => 1, DIR => 'tmp');
        #warn "Grammar File: $gmfile";
        print $fh $gm;
        close $fh;

        my $meta_opts = $block->meta_opts || '';
        my @cmd = ($^X, 'LL1.pl');
        if ($meta_opts) {
            push @cmd, (split /\s+/, $meta_opts);
        }
        push @cmd, $gmfile;
        is system(@cmd), 0, "$name - LL1.pl";
        if ($meta_opts =~ /-m/) {
            ($plfile = $gmfile) =~ s/\.grammar$/.pm/;
        } else {
            ($plfile = $gmfile) =~ s/\.grammar$/.pl/;
        }
        ok -f $plfile, "$name - $plfile ok";
    }

    if ($plfile =~ /\.pl$/) {
        my ($fh, $infile) = tempfile('in_XXXXXX', UNLINK => 1, DIR => 'tmp');
        #warn "Input File: $infile";
        print $fh $input;
        close $fh;
        ok -f $infile, "$name - $infile ok";
        my ($stdout, $stderr);
        my @cmd = ($^X, $plfile);
        if ($opts) {
            push @cmd, (split /\s+/, $opts);
        }
        push @cmd, $infile;
        run3 \@cmd, \undef, \$stdout, \$stderr;
        if (defined $block->stderr) {
            is $stderr, $block->stderr, "$name - stderr ok";
        }
        if (defined $block->stdout) {
            is $stdout, $block->stdout, "$name - stdout ok";
        }
    } else {
        is system($^X, $plfile), 0, "$name - pm file eval ok";
    }
    push @plfiles, $plfile;
};

for my $plfile (@plfiles) {
    unlink $plfile;
}

__DATA__

=== TEST 1: Basic
--- grammar

identifier: /[A-Za-z]\w*/

--- input
32 foo
--- stdout
--- stderr
Was expecting identifier, but found '32' instead at offset 0.



=== TEST 2: Test the tracing feature of the generated parsers
--- opts
-d
--- input
bar32
--- stdout
  read token /[A-Za-z]\w*/
trying identifier...    [bar32\n]
  generate { identifier -> /[A-Za-z]\w*/ }
trying /[A-Za-z]\w*/...    [bar32\n]
  >>MATCH<< /[A-Za-z]\w*/ with 'bar32'
  read token /\Z/
  >>ACCEPT<<

success.

--- stderr



=== TEST 3: Test the -n option of LL1.pl
--- meta_opts
-n ABC
--- grammar

number: /\d+/

--- input

  321

--- stdout
1
--- stderr



=== TEST 4: Test the -m option of LL1.pl
--- meta_opts
-m
--- grammar

if_stmt: 'if' '(' exp ')' statement
exp: /\d+/
statement: /\S+/

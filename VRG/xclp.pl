use strict;
use warnings;

use CLIPSx;
use List::MoreUtils 'uniq';
use File::Slurp;
use Parse::RecDescent;
use Data::Dump::Streamer;

our $infile = shift or
    die "usage: $0 infile\n";
(my $outfile = $infile) =~ s/\.xclp$/.clp/i;
$outfile .= '.clp' if $outfile !~ /\.clp$/i;

our ($base) = ($outfile =~ /^([\w-]+)/);
$base = "f$base" if $base !~ /^[A-Za-z_]/;

my $source = read_file($infile);

our $count;
our @facts;
our $rel_type;
our $module;

our (%prefix, %infix, %infix_prefix);

$::RD_HINT = 1;
#$::RD_TRACE = 1;
our $parser = CLIPSx->new;
my $data = $parser->program($source);
if (!defined $data) {
    die "abort.\n";
}
$data .= "\n" if $data and $data !~ /\n$/s;
if (@facts) {
    $data .= "(deffacts $base\n    " . join("\n    ", uniq @facts). ")\n";
}
$data .= "\n" if $data and $data !~ /\n$/s;

sub match_prefix {
    my @keys = sort { -($a cmp $b) } keys %::prefix;
    return match($_[0], \@keys);
}

sub match_infix {
    my @keys = sort { -($a cmp $b) } keys %::infix;
    return match($_[0], \@keys);
}

sub match {
    $_[0] =~ s/^\s+//;
    my $rkeys = pop;
    for my $key (@$rkeys) {
        #warn "$key => ", $::prefix{$key}, "\n";
        my $len = length($key);
        if (substr($_[0], 0, $len) eq $key) {
            #warn "!!! matched prefix <$key>";
            $_[0] = substr($_[0], $len);
            return $key;
        }
    }
    return undef;
}

sub process_include {
    my ($fname, $linno) = @_;
    my $src = read_file($fname);
    my $saved_infile = $::infile;
    local $::infile = $fname;
    my $parser = CLIPSx->new;
    my $data = $parser->program($src);
    if (!defined $data) {
        die "error: $saved_infile (line $linno): can't include file $fname.\n";
    }
    #warn "$data!!!";
    $data;
}

if ($data) {
    write_file($outfile, $data);
    #print $data;
}

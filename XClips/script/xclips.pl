use strict;
use warnings;

use Getopt::Long;
use FindBin;
use XClips::Compiler;
use List::MoreUtils 'uniq';
use File::Slurp;
use Data::Dump::Streamer;

GetOptions(
    'I=s'   => \@::Include,
    'c'     => \my $compile_only,
    'trim'  => \my $trim,
    'debug' => \my $debug,
) or help();

$::RD_TRACE = 1 if $debug;

sub help {
    die "usage: $0 [-I dir] infile\n";
}

my @infiles = map glob, @ARGV or help();

my @outfiles;

for my $infile (@infiles) {
    compile_file($infile);
}

if (!$compile_only) {
    run_clips();
}

sub compile_file {
    my $infile = shift;
    my $outfile;

    if ($infile !~ /\.clp$/i) {
        ($outfile = $infile) =~ s/\.xclp$/.clp/i;
        $outfile .= '.clp' if $outfile !~ /\.clp$/i;

        our ($base) = ($outfile =~ /([\w-]+)\.\w+$/);
        $base = "f$base" if $base !~ /^[A-Za-z_]/;

        my $source = read_file($infile);

        $::RD_HINT = 1;
        #$::RD_TRACE = 1;
        our $parser = XClips::Compiler->new;
        my $data = $parser->program($source);
        if (!defined $data) {
            die "abort.\n";
        }
        $data .= "\n" if $data and $data !~ /\n$/s;
        if (@::facts) {
            $data .= "(deffacts $base\n    " . join("\n    ", uniq @::facts). ")\n";
        }
        $data .= "\n" if $data and $data !~ /\n$/s;

        #my @elems = %infix_circumfix;
        #warn "\%infix_circumfix: @elems\n";

        #@elems = %infix_circum_close;
        #warn "\%infix_circum_close: @elems\n";

        if ($data) {
            write_file($outfile, $data);
            #print $data;
        }
    } else {
        $outfile = $infile;
    }

    push @outfiles, $outfile;
    #warn "OUT: @outfiles\n";
}


sub run_clips {
    if ($debug) {
        require Clips::Batch;
        require Clips::GraphViz;
        my $clips = Clips::Batch->new(@outfiles);
        $clips->watch('facts');
        $clips->watch('rules');
        $clips->reset;
        $clips->facts('*', \my $init_facts);
        $clips->rules('*', \my $rules);
        $clips->run(\my $run_log);
        $clips->eof;
        my $painter = Clips::GraphViz->new($init_facts, $run_log);
        my $outfile = 'a.png';
        $painter->draw(
            outfile => $outfile,
            trim    => $trim,
        );
        warn "generating $outfile...\n";

        require YAML::Syck;
        my $db_dir = 'clips_cover_db';
        mkdir $db_dir if !-d $db_dir;
        my ($fname, $time);
        $time = time;
        while (my $rand = int rand 1000000) {
            $fname = "$db_dir/$time-$rand.yml";
            last if !-e $fname;
        }
        YAML::Syck::DumpFile($fname, [$rules, $run_log]);
    } else {
        require Clips::Batch;
        my $clips = Clips::Batch->new(@outfiles);
        $clips->reset;
        $clips->run(sub { print $_[0] if defined $_[0] });
        $clips->eof;
    }
}    

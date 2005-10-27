use SQL::Translator;
use Getopt::Std;

my %opts;
getopts('o:f:', \%opts);

my $format = $opts{f} || 'HTML';
my $outfile = $opts{o} || '-';

#my @files = map glob, @ARGV;
my $file = shift;
die "No input file specified.\n" unless $file;

#warn $file;
my $translator          = SQL::Translator->new(
    # Print debug info
    debug               => 0,
    # Print Parse::RecDescent trace
    trace               => 0,
    # Don't include comments in output
    no_comments         => 1,
    # Print name mutations, conflicts
    show_warnings       => 0,
    # Add "drop table" statements
    add_drop_table      => 1,
    # Validate schema object
    validate            => 1,
);

my $proc_args;
if ($format eq 'HTML') {
    $proc_args = { pretty => 1};
} else {
    $proc_args = {
        add_color => 1,
        show_constraints => 1,
        show_datatypes => 1,
        show_col_sizes => 1
    };
}

my $output     = $translator->translate(
    from       => 'MySQL',
    to         => $format,
    # Or an arrayref of filenames, i.e. [ $file1, $file2, $file3 ]
    filename   => $file,
    producer_args => $proc_args,
) or $translator->error;

unslurp($outfile, $output);

sub unslurp {
    my ($file, $content) = @_;
    open my $fh, ">$file" or
        die "Can't open $file for writing: $!";
    binmode $fh;
    print $fh $content;
    close $fh;
}

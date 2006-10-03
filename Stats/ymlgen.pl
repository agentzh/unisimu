use strict;
use warnings;
use YAML::Syck;

my @files = map glob, @ARGV;
for my $file (@files) {
    process_file($file);
}

sub process_file {
    my $file = shift;
    
    open my $in, $file or
        die "error: Cannot read $file: $!\n";
    local $/;
    my $src = <$in>;
    close $in;
    
    unless ($src =~ s/(?:\{\-)?\s*__END__\s*\n(.*)$//so) {
        warn "warning: Can't find __END__ in $file hence skip it.\n";
        return;
    }
    my $problem = $1;
    $problem =~ s/^(Example \w+)\n\n/B<$1>\n\n/gms;
    $problem =~ s/-\}\s*\n$//s if $file =~ /\.hs$/i;

    my $output;
    if ($file =~ /\.pl$/) {
        $output = `perl $file`;
    } elsif ($file =~ /\.hs$/) {
        $output = `$file.exe`;
    }

    $output =~ s/^([^\n])/    $1/gosm;
    $src =~ s/^([^\n])/    $1/gosm;

    DumpFile("$file.yml", {
        file => $file,
        problem => $problem,
        src => $src,
        output => $output,
    });
    warn "$file.yml generated.\n";
}

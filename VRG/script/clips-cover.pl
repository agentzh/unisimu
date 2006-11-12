use strict;
use warnings;

use Getopt::Std;
use Text::Table;
use YAML::Syck;

my %opts;
getopts('d', \%opts);

my $db_dir = 'clips_cover_db';

if ($opts{d}) {
    my @files = glob "$db_dir/*.yml";
    for (@files) {
        unlink($_);
    }
    rmdir $db_dir;
    exit(0);
}

my $total_fires;
my %rules;

if (!-d $db_dir) {
    die "No Coverage Database found.\n";
}
opendir my $dh, $db_dir or
    die "error: Can't open $db_dir for reading: $!";
while (my $entry = readdir($dh)) {
    next if -d $entry or $entry !~ /\.yml$/i;
    my $data = LoadFile("$db_dir/$entry");
    #warn $data;
    my ($rules, $fires) = @$data;
    parse_rule_list($rules);
    parse_fire_list($fires);
}
closedir $dh;

my $hit;
my @stats;
while (my ($rule_name, $count) = each %rules) {
    push @stats, [$rule_name, $count];
    $hit++ if $count > 0;
}
@stats = sort {
    my $res = $a->[1] <=> $b->[1];
    ($res == 0) ? ($a->[0] cmp $b->[0]) : $res;
} @stats;

my $tb = Text::Table->new(
        "Rule", "Count"
);

$tb->load(@stats);
print $tb->rule( '-' );
print $tb->title;
print $tb->rule( '-' );
print $tb->body;
printf("\nFor total %.02f%% of the rules have been fired.\n",
    "$hit.0"/scalar(keys %rules)*100);

sub parse_rule_list {
    my $log = shift;
    open my $in, '<', \$log or die;
    while (<$in>) {
        if (/^\s+(\S+)$/) {
            my $rule_name = $1;
            $rules{$rule_name} = 0 if !exists $rules{$rule_name};
        }
    }
    close $in;
}

sub parse_fire_list {
    my $log = shift;
    open my $in, '<', \$log or die;
    while (<$in>) {
        if (/(?x) ^ FIRE \s+ \d+ \s+ (\S+):/) {
            my $rule_name = $1;
            $rules{$rule_name}++;
            $total_fires++;
        }
    }
}

#: insert-data.pl
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-12 2005-11-19

use strict;
use warnings;
use DBI;
use Getopt::Std;

my %opts;
getopts('u:p:', \%opts);

my ($user, $password) = ($opts{u}, $opts{p});

@ARGV or die "No CSV file given.\n";

my $dsn = $ENV{DSN};
die "No env DSN set.\n" unless $dsn;

my $dbh;
if ($user) {
	$dbh = DBI->connect($dsn, $user, $password, { PrintError => 1 });
} else {
	$dbh = DBI->connect($dsn, { PrintError => 1 });
}

my @files = map glob, @ARGV;

foreach (@files) {
	process_csv(shift);
}

sub process_csv {
	my $csvfile = shift;
	my $table;
	if ($csvfile =~ /(\w+)\.\w+$/) {
		$table = $1;
	} else {
		die "Invalid file name.\n";
	}

	open my $in, $csvfile or
		die "error: Can't open $csvfile: $!\n";
	$_ = <$in>;
	chomp;
	my @flds = split /,/, $_;
	my $nflds = @flds;
	my @place_holders = map { '?' } @flds;
	my $fld_list = join(',', @flds);

	my $sql = "insert into $table ($fld_list) values (".join(',',@place_holders).')';
	warn "  sql: $sql\n";
	my $sth = $dbh->prepare($sql);

	warn "info: Inserting data to $table...\n";
	while (<$in>) {
		chomp;
		@flds = split /\s*,\s*/, $_;
		@flds = map { s/^\s*"(.*)"\s*$/$1/; $_ } @flds;
		@flds = map { $_ eq '<NULL>' ? undef : $_ } @flds;
		$" = ':';
		#print "*@flds*\n";
		if (not $sth->execute(@flds)) {
			last;
		}
	}
}

$dbh->disconnect();

__END__

=head1 NAME

csv2db - Import CSV file to databases

=head1 USAGE

	csv2db -u foo -p 12345 Students.csv Instructors.csv
	csv2db Banks.csv

=head1 CSV File Format

The name of the input CSV file must match the name of the
target database table.

The first line should be the list of field names, for example,

	stu_id,stu_name,stu_sex

where the order is arbitrary.

You can use double quotes but they're not necessary.

If you want to specify NULL in your CSV input file, you can use the
syntax "E<lt>NULLE<gt>". Note that it is case-sensitive.

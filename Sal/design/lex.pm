

package lex;
require Exporter;
@ISA = qw/Exporter/;
@EXPORT = qw/get_words/;

use strict;
use warnings;

sub get_words{
	my $file = shift || die "input grammar file!\n";
	open my $f, $file or die "cannot open $file to read: $!\n";
	local $/;
	local $_ = <$f>;
	close $f or die $!, "\n";
	my $line = 1;
	my @words;
	while (1) {


		if(/\G(if)\b/gc) {
				push @words, ['if', 'if', $line];
		}
		elsif(/\G(then)\b/gc) {
				push @words, ['then', 'then', $line];
		}
		elsif(/\G(else)\b/gc) {
				push @words, ['else', 'else', $line];
		}
		elsif(/\G(end)\b/gc) {
				push @words, ['end', 'end', $line];
		}
		elsif(/\G(repeat)\b/gc) {
				push @words, ['repeat', 'repeat', $line];
		}
		elsif(/\G(until)\b/gc) {
				push @words, ['until', 'until', $line];
		}
		elsif(/\G(read)\b/gc) {
				push @words, ['read', 'read', $line];
		}
		elsif(/\G(write)\b/gc) {
				push @words, ['write', 'write', $line];
		}
		elsif(/\G(\+)/gc) {
				push @words, ['+', '+', $line];
		}
		elsif(/\G(\-)/gc) {
				push @words, ['-', '-', $line];
		}
		elsif(/\G(\*)/gc) {
				push @words, ['*', '*', $line];
		}
		elsif(/\G(\/)/gc) {
				push @words, ['/', '/', $line];
		}
		elsif(/\G(\=)/gc) {
				push @words, ['=', '=', $line];
		}
		elsif(/\G(\<)/gc) {
				push @words, ['<', '<', $line];
		}
		elsif(/\G(\()/gc) {
				push @words, ['(', '(', $line];
		}
		elsif(/\G(\))/gc) {
				push @words, [')', ')', $line];
		}
		elsif(/\G(\;)/gc) {
				push @words, [';', ';', $line];
		}
		elsif(/\G(\:=)/gc) {
				push @words, [':=', ':=', $line];
		}
		elsif(/\G(\d+)\b/gc) {
				push @words, [$1, 'number', $line];
		}
		elsif(/\G([A-Za-z]\w*)\b/gc) {
				push @words, [$1, 'identifier', $line];
		}
		elsif(/\G(\s*)/gc) {
			$line++ if $1 =~ m/\n/;
		}
		elsif (/\G(\S+)/gc) {
			print "ERROR: $1\n"
		}
		else {
			last;
		}
	}
	return @words;
}

1;

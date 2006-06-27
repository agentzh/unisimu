#genlexer.pl is written for generating lex.pm
#which parses source code of tiny language
#version 0.0.2
#2006-06-25 2006-06-25

my($num, $id) = ('\d+', '[A-Za-z]\w*');		

#my @table = (
#		['if', 1], ['then', 2], ['else', 3], ['end', 4], ['repeat', 5], ['until', 6], ['read' ,7], ['write' ,8],
#		['+' ,8], ['-' ,9], ['*' ,10], ['/' ,11], ['=' ,12], ['<' ,13], ['(' ,14], [')' ,15], [';' ,16], [':=' , 17],
#		[$num ,18],[$id, 19]

#);
my @table = (
		['if', 'if'], ['then', 'then'], ['else', 'else'], ['end', 'end'], ['repeat', 'repeat'], ['until', 'until'], ['read' ,'read'], ['write' ,'write'],
		['+' ,'+'], ['-' ,'-'], ['*' ,'*'], ['/' ,'/'], ['=' ,'='], ['<' ,'<'], ['(' ,'('], [')' ,')'], [';' ,';'], [':=' , ':='],
		[$num ,'number'],[$id, 'identifier']
);

my @meta = ('+', '-', '*', '(', ')', '[', ']', '?', '/', '<', ';', ':=', '=');
my $body = "";


for my $i (0..@table-1) {
	#print $i;
	my $start;
	my ($token, $dfa, $value);
	$value = $table[$i][1];
	$dfa = $table[$i][0];
	if($dfa eq $num || $dfa eq $id) {
		$token = '$1';
		$dfa .= ')\b';
	} else {
		$token = "'".$dfa."'";		
		if(is_meta($dfa)) {
			$dfa = '\\'.$dfa.')';
		} else {
			$dfa .= ')\b';
		}
		
	}
	if($i) {
		$start = 'elsif';
	} else {
		$start = "if";
	}
	my $template = '
		%START%(/\G(%DFA%/gc) {
				push @words, [%TOKEN%, \'%VALUE%\', $line];
		}';
	$template =~ s/%START%/$start/g;
	$template =~ s/%DFA%/$dfa/g;
	$template  =~ s/%TOKEN%/$token/g;
	$template =~ s/%VALUE%/$value/g;
	$body .= $template;
}

#print $string;

sub is_meta {
	my $s = shift;
	for(@meta) {
		return 1 if $s eq $_;
	}
	return;
}

my $head = '

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

';

my $tail = '
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
';

my $program = $head. $body. $tail;

print $program;

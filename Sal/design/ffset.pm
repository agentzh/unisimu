#ffset2.pm written for get first and follow set
#of every nonterminal and terminal in production
#if necessary through object_oriented method
#version 0.0.6 
#2006-6-22 2006-6-24

package Ffset;

use strict;
use warnings;
use Data::Dumper;

my $sstate = 'E';
my (@productions, %production, %followset, @non_terminal, %tag);

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self = {};
	
	bless ($self, $class);
	return $self;
}

sub set_sstate {
	shift;
	my $s = shift;
	$sstate = $s;
}

sub get_prod {
	my $self = shift;
	@productions = @_;
	$self->_build;
}


sub _build {
	shift;
	for(@productions) {
		my($key, $value) = split /\s+\->\s+/, $_;
		if(exists $production{$key}) {
			push @{$production{$key}}, $value;
			push @non_terminal, $key;
		} else {
			$production{$key} = [$value];
			$tag{$key} = 1;
		}
	}
	for my $key(keys %production) {
		for(@productions) {
			#print $_, "\n";
			if(/\->.*\s+$key(\s|$)/) {
				if(exists $followset{$key}) {
					push @{$followset{$key}}, $_;
				} else {
					$followset{$key} = [$_];
				}
			}
		}
	}
}


sub get_terminal {
	shift;
	return @non_terminal;
}

sub get_first {
	my $self = shift;
	my $str = shift;		
	my @fset;

	if(exists $production{$str}) {
		if($tag{$str} == 1) {
			my @rightv = $self->_extend($str);
			for(@rightv) {
				#next if /^$str(\s+|$)/;
				$tag{$str} = 0;
				$self->_put(\@fset, 0, $self->get_first($_));
				$tag{$str} = 1;
			}
		}
		return @fset;
	}

	my @leftv = split /\s+/, $str;
	my $mark = 0;
	for(@leftv) {
		$mark++;
		if($self->_is_terminal($_)) {
			$self->_put(\@fset, 0, $_);
			
		} else {
			my @rightv = $self->_extend($_);
			my @set;
			for my $inner(@rightv) {
				
				#print $inner, "\t";
				push @set, $self->get_first($inner) unless $inner eq $_;
				#print @set, "\n";
			}
			$self->_put(\@fset, 1, @set);
			
			if($self->has_epsilon(@set)) {
				if(@leftv > 1 && $mark == @leftv) {
					$self->_put(\@fset, 0, '@');
				}
				next;
			} 
		}
		last;
	}
	return @fset;
}

sub get_follow {
	my $self = shift;
	my $non = shift;
	my @follow;
	#print $non, "\n";
	if($tag{$non} == 1) {
		if($non eq $sstate) {
			$self->_put(\@follow, 0, '#');
		}
		for(@{$followset{$non}}) {
			/^(.*?)\s+\->.*?$non(?:\s+(\S+)|$)/;
			#print $1, "\t", $2, "\n";
			if($2) {
				my @fset = $self->get_first($2);
				if($self->has_epsilon(@fset)) {
					$tag{$non} = 0;
					$self->_put(\@follow, 1, $self->get_follow($1)) unless $1 eq $non;
					$tag{$non} = 1;
				}
				$self->_put(\@follow, 1, @fset);		
			} else {
				$tag{$non} = 0;
				$self->_put(\@follow, 1, $self->get_follow($1)) unless $1 eq $non;
				$tag{$non} = 1;
			}
		
		}
	}
	return @follow;
}


sub _is_terminal {
	shift;
	my $e = shift;
	#print $e, "\n";
	for(keys %production) {
		#print $$_[0], "\t";
		return  if $_ eq $e;
	}
	return 1;
}

sub _extend {
	shift;
	my $key = shift;
	return @{$production{$key}};
}

sub has_epsilon {
	shift;
	my @set = @_;
	for(@set) {
		return 1 if $_ eq '@';
	}
	return;
}

sub _put {
	shift;
	my($des, $t, @res) = @_;
	my $en = 1;
	for my $e (@res) {
		$en = 0 if $e eq '@' && $t == 1;
		for(@$des) {
			$en = 0 if $e eq $_;
		}
		push @$des, $e if $en == 1;
	}
}

1;


=pod

my @prods = (
				'program	    -> stmt-sequence',
				'stmt-sequence  -> statement stmt-sequence@',
				'stmt-sequence@ -> ; statement stmt-sequence',
				'stmt-sequence@ -> @',
				'statement		-> if-stmt',
				'statement		-> repeat-stmt',
				'statement		-> assign-stmt',
				'statement		-> read-stmt',
				'statement		-> write-stmt',
				#'statement		-> @',
				'if-stmt		-> if exp then stmt-sequence if-stmt@',
				'if-stmt@		-> end',
				'if-stmt@		-> else stmt-sequence end',
				#'if-stmt		-> if exp then stmt-sequence end',
				#'if-stmt		-> if exp then stmt-sequence else stmt-sequence end',
				'repeat-stmt	-> repeat stmt-sequence until exp',
				'assign-stmt	-> identifier := exp',
				'read-stmt		-> read identifier',
				'write-stmt		-> write exp',
				#'exp			-> simple-exp comparison-op simple-exp',
				#'exp			-> simple-exp',
				'exp			-> simple-exp exp@',
				'exp@			-> comparison-op simple-exp exp@',
				'exp@			-> @',
				'comparison-op	-> <',
				'comparsion-op	-> =',
				'simple-exp		-> term simple-exp@',
				'simple-exp@	-> addop term simple-exp@',
				'simple-exp@	-> @',
				#'simple-exp		-> simple-exp addop term',
				#'simple-exp		-> term',
				'addop			-> +',
				'addop			-> -',
				'term			-> factor term@',
				'term@			-> mulop factor term@',
				'term@			-> @',
				'mulop			-> *',
				'mulop			-> /',
				'factor		-> ( exp )',
				'factor		-> number',
				'factor		-> identifier',
);


my $obj = Ffset->new();
$obj->get_prod(@prods);
$obj->set_sstate('program');
print "first set: ", join "\n", $obj->get_first('exp'), "\n";
print "follow set: ", join "\n", $obj->get_follow('exp'), "\n";
#print Data::Dumper::Dumper(\%tag);
#my @non = ('program','stmt-sequence','stmt-sequence@','statement','statement','if-stmt','if-stmt@','repeat-stmt',
#		   'assign-stmt','read-stmt','write-stmt','exp','comparison-op','simple-exp','simple-exp@','addop','term',
#		   'mulop','mulop','factor');		
#for(@non) {
#	print "$_ follow set: ", join "\n", $obj->get_follow($_), "\n";
#}

=cut

	

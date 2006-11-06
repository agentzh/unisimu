package CLIPSx;
use Parse::RecDescent;

{ my $ERRORS;


package Parse::RecDescent::CLIPSx;
use strict;
use vars qw($skip $AUTOLOAD  );
$skip = '\s*';


{
local $SIG{__WARN__} = sub {0};
# PRETEND TO BE IN Parse::RecDescent NAMESPACE
*Parse::RecDescent::CLIPSx::AUTOLOAD	= sub
{
	no strict 'refs';
	$AUTOLOAD =~ s/^Parse::RecDescent::CLIPSx/Parse::RecDescent/;
	goto &{$AUTOLOAD};
}
}

push @Parse::RecDescent::CLIPSx::ISA, 'Parse::RecDescent';
# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::number
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"number"};
	
	Parse::RecDescent::_trace(q{Trying rule: [number]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{number},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [/\\d+(?:\\.\\d*)?/]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{number},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{number});
		%item = (__RULE__ => q{number});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/\\d+(?:\\.\\d*)?/]}, Parse::RecDescent::_tracefirst($text),
					  q{number},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:\d+(?:\.\d*)?)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [/\\d+(?:\\.\\d*)?/]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{number},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [/\\.\\d+/]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{number},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{number});
		%item = (__RULE__ => q{number});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/\\.\\d+/]}, Parse::RecDescent::_tracefirst($text),
					  q{number},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:\.\d+)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [/\\.\\d+/]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{number},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{number},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{number},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{number},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{number},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::variable
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"variable"};
	
	Parse::RecDescent::_trace(q{Trying rule: [variable]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{variable},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [/\\?[A-Za-z_]([-\\w])*/]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{variable},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{variable});
		%item = (__RULE__ => q{variable});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/\\?[A-Za-z_]([-\\w])*/]}, Parse::RecDescent::_tracefirst($text),
					  q{variable},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:\?[A-Za-z_]([-\w])*)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{variable},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { $item[1] . $item{identifier} };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [/\\?[A-Za-z_]([-\\w])*/]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{variable},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: ['?']},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{variable},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{variable});
		%item = (__RULE__ => q{variable});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['?']},
					  Parse::RecDescent::_tracefirst($text),
					  q{variable},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\?//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: ['?']<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{variable},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{variable},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{variable},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{variable},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{variable},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::fact
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"fact"};
	
	Parse::RecDescent::_trace(q{Trying rule: [fact]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{fact},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [clause]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{fact},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{fact});
		%item = (__RULE__ => q{fact});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [clause]},
				  Parse::RecDescent::_tracefirst($text),
				  q{fact},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::clause($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [clause]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{fact},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [clause]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{fact},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{clause}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{fact},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { push @::facts, "; $::infile (line $itempos[1]{line}{from} ~ ".
                    "$itempos[1]{line}{to})",
                    "$item{clause}\n"; '' };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [clause]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{fact},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{fact},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{fact},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{fact},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{fact},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::string
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"string"};
	
	Parse::RecDescent::_trace(q{Trying rule: [string]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{string},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: []},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{string},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{string});
		%item = (__RULE__ => q{string});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{string},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { extract_delimited($text, '"') };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: []<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{string},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{string},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{string},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{string},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{string},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::rule
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"rule"};
	
	Parse::RecDescent::_trace(q{Trying rule: [rule]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{rule},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [disjunction '=>' <commit> new_facts '.' /[\\n\\s]*/]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{rule});
		%item = (__RULE__ => q{rule});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [disjunction]},
				  Parse::RecDescent::_tracefirst($text),
				  q{rule},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::disjunction($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [disjunction]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{rule},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [disjunction]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{disjunction}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['=>']},
					  Parse::RecDescent::_tracefirst($text),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{'=>'})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\=\>//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<commit>]},
					Parse::RecDescent::_tracefirst($text),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { $commit = 1 };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [new_facts]},
				  Parse::RecDescent::_tracefirst($text),
				  q{rule},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{new_facts})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::new_facts($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [new_facts]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{rule},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [new_facts]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{new_facts}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['.']},
					  Parse::RecDescent::_tracefirst($text),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{'.'})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\.//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING2__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/[\\n\\s]*/]}, Parse::RecDescent::_tracefirst($text),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{/[\\n\\s]*/})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:[\n\s]*)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { $::count++;
          "; $::infile (line $itempos[1]{line}{from} ~ $itempos[5]{line}{to})\n".
          "(defrule $::module$::base-$::count\n".
          "    $item[1]\n".
          "    =>\n".
          "$item[4])" };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [disjunction '=>' <commit> new_facts '.' /[\\n\\s]*/]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [<error?:...> <reject>]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		
		my $_savetext;
		@item = (q{rule});
		%item = (__RULE__ => q{rule});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<error?:...>]},
					Parse::RecDescent::_tracefirst($text),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { if ($commit) { do {
		my $rule = $item[0];
		   $rule =~ s/_/ /g;
		#WAS: Parse::RecDescent::_error("Invalid $rule: " . $expectation->message() ,$thisline);
		push @{$thisparser->{errors}}, ["Invalid $rule: " . $expectation->message() ,$thisline];
		} unless  $_noactions; undef } else {0} };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{>>Rejecting production<< (found <reject>)},
					 Parse::RecDescent::_tracefirst($text),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE;
		undef $return;
		

		$_tok = undef;
		
		last unless defined $_tok;

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [<error?:...> <reject>]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{rule},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{rule},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{rule},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{rule},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::program
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"program"};
	
	Parse::RecDescent::_trace(q{Trying rule: [program]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{program},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [statement eofile]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{program},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{program});
		%item = (__RULE__ => q{program});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying repeated subrule: [statement]},
				  Parse::RecDescent::_tracefirst($text),
				  q{program},
				  $tracelevel)
					if defined $::RD_TRACE;
		$expectation->is(q{})->at($text);
		
		unless (defined ($_tok = $thisparser->_parserepeat($text, \&Parse::RecDescent::CLIPSx::statement, 1, 100000000, $_noactions,$expectation,undef))) 
		{
			Parse::RecDescent::_trace(q{<<Didn't match repeated subrule: [statement]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{program},
						  $tracelevel)
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched repeated subrule: [statement]<< (}
					. @$_tok . q{ times)},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{program},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{statement(s)}} = $_tok;
		push @item, $_tok;
		


		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [eofile]},
				  Parse::RecDescent::_tracefirst($text),
				  q{program},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{eofile})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::eofile($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [eofile]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{program},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [eofile]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{program},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{eofile}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{program},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { join "\n\n", grep $_, @{ $item[1] } };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [statement eofile]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{program},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [<error...>]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{program},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		
		my $_savetext;
		@item = (q{program});
		%item = (__RULE__ => q{program});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<error...>]},
					Parse::RecDescent::_tracefirst($text),
					  q{program},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { if (1) { do {
		my $rule = $item[0];
		   $rule =~ s/_/ /g;
		#WAS: Parse::RecDescent::_error("Invalid $rule: " . $expectation->message() ,$thisline);
		push @{$thisparser->{errors}}, ["Invalid $rule: " . $expectation->message() ,$thisline];
		} unless  $_noactions; undef } else {0} };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [<error...>]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{program},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{program},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{program},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{program},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{program},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::postfix
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"postfix"};
	
	Parse::RecDescent::_trace(q{Trying rule: [postfix]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{postfix},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: ['postfix']},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{postfix},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{postfix});
		%item = (__RULE__ => q{postfix});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['postfix']},
					  Parse::RecDescent::_tracefirst($text),
					  q{postfix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\Apostfix//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: ['postfix']<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{postfix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{postfix},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{postfix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{postfix},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{postfix},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::literal
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"literal"};
	
	Parse::RecDescent::_trace(q{Trying rule: [literal]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{literal},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [identifier]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{literal},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{literal});
		%item = (__RULE__ => q{literal});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [identifier]},
				  Parse::RecDescent::_tracefirst($text),
				  q{literal},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::identifier($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [identifier]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{literal},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [identifier]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{literal},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{identifier}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [identifier]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{literal},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [number]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{literal},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{literal});
		%item = (__RULE__ => q{literal});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [number]},
				  Parse::RecDescent::_tracefirst($text),
				  q{literal},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::number($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [number]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{literal},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [number]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{literal},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{number}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [number]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{literal},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [string]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{literal},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[2];
		$text = $_[1];
		my $_savetext;
		@item = (q{literal});
		%item = (__RULE__ => q{literal});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [string]},
				  Parse::RecDescent::_tracefirst($text),
				  q{literal},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::string($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [string]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{literal},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [string]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{literal},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{string}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [string]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{literal},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{literal},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{literal},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{literal},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{literal},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::identifier
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"identifier"};
	
	Parse::RecDescent::_trace(q{Trying rule: [identifier]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{identifier},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [/[A-Za-z_]([-\\w])*/]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{identifier},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{identifier});
		%item = (__RULE__ => q{identifier});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/[A-Za-z_]([-\\w])*/]}, Parse::RecDescent::_tracefirst($text),
					  q{identifier},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:[A-Za-z_]([-\w])*)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [/[A-Za-z_]([-\\w])*/]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{identifier},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{identifier},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{identifier},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{identifier},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{identifier},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::statement
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"statement"};
	
	Parse::RecDescent::_trace(q{Trying rule: [statement]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{statement},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [comment]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{statement});
		%item = (__RULE__ => q{statement});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [comment]},
				  Parse::RecDescent::_tracefirst($text),
				  q{statement},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::comment($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [comment]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{statement},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [comment]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{comment}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [comment]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [directive]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{statement});
		%item = (__RULE__ => q{statement});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [directive]},
				  Parse::RecDescent::_tracefirst($text),
				  q{statement},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::directive($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [directive]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{statement},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [directive]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{directive}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [directive]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [rule]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[2];
		$text = $_[1];
		my $_savetext;
		@item = (q{statement});
		%item = (__RULE__ => q{statement});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [rule]},
				  Parse::RecDescent::_tracefirst($text),
				  q{statement},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::rule($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [rule]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{statement},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [rule]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{rule}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [rule]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [facts '.' /[\\n\\s]*/]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[3];
		$text = $_[1];
		my $_savetext;
		@item = (q{statement});
		%item = (__RULE__ => q{statement});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [facts]},
				  Parse::RecDescent::_tracefirst($text),
				  q{statement},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::facts($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [facts]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{statement},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [facts]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{facts}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['.']},
					  Parse::RecDescent::_tracefirst($text),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{'.'})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\.//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/[\\n\\s]*/]}, Parse::RecDescent::_tracefirst($text),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{/[\\n\\s]*/})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:[\n\s]*)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { '' };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [facts '.' /[\\n\\s]*/]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{statement},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{statement},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{statement},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{statement},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::predicate
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"predicate"};
	
	Parse::RecDescent::_trace(q{Trying rule: [predicate]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{predicate},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [identifier '(' <commit> arguments ')']},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{predicate});
		%item = (__RULE__ => q{predicate});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [identifier]},
				  Parse::RecDescent::_tracefirst($text),
				  q{predicate},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::identifier($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [identifier]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{predicate},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [identifier]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{identifier}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['(']},
					  Parse::RecDescent::_tracefirst($text),
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{'('})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\(//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<commit>]},
					Parse::RecDescent::_tracefirst($text),
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { $commit = 1 };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [arguments]},
				  Parse::RecDescent::_tracefirst($text),
				  q{predicate},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{arguments})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::arguments($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [arguments]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{predicate},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [arguments]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{arguments}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [')']},
					  Parse::RecDescent::_tracefirst($text),
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{')'})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING2__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { "($item[1] $item{arguments})" };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [identifier '(' <commit> arguments ')']<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [<error?:...> <reject>]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		
		my $_savetext;
		@item = (q{predicate});
		%item = (__RULE__ => q{predicate});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<error?:...>]},
					Parse::RecDescent::_tracefirst($text),
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { if ($commit) { do {
		my $rule = $item[0];
		   $rule =~ s/_/ /g;
		#WAS: Parse::RecDescent::_error("Invalid $rule: " . $expectation->message() ,$thisline);
		push @{$thisparser->{errors}}, ["Invalid $rule: " . $expectation->message() ,$thisline];
		} unless  $_noactions; undef } else {0} };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{>>Rejecting production<< (found <reject>)},
					 Parse::RecDescent::_tracefirst($text),
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE;
		undef $return;
		

		$_tok = undef;
		
		last unless defined $_tok;

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [<error?:...> <reject>]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{predicate},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{predicate},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{predicate},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{predicate},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::facts
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"facts"};
	
	Parse::RecDescent::_trace(q{Trying rule: [facts]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{facts},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [fact ',' <commit> facts]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{facts});
		%item = (__RULE__ => q{facts});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [fact]},
				  Parse::RecDescent::_tracefirst($text),
				  q{facts},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::fact($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [fact]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{facts},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [fact]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{fact}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [',']},
					  Parse::RecDescent::_tracefirst($text),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{','})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\,//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<commit>]},
					Parse::RecDescent::_tracefirst($text),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { $commit = 1 };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [facts]},
				  Parse::RecDescent::_tracefirst($text),
				  q{facts},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{facts})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::facts($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [facts]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{facts},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [facts]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{facts}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [fact ',' <commit> facts]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [fact]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{facts});
		%item = (__RULE__ => q{facts});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [fact]},
				  Parse::RecDescent::_tracefirst($text),
				  q{facts},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::fact($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [fact]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{facts},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [fact]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{fact}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [fact]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [<error?:...> <reject>]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[2];
		
		my $_savetext;
		@item = (q{facts});
		%item = (__RULE__ => q{facts});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<error?:...>]},
					Parse::RecDescent::_tracefirst($text),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { if ($commit) { do {
		my $rule = $item[0];
		   $rule =~ s/_/ /g;
		#WAS: Parse::RecDescent::_error("Invalid $rule: " . $expectation->message() ,$thisline);
		push @{$thisparser->{errors}}, ["Invalid $rule: " . $expectation->message() ,$thisline];
		} unless  $_noactions; undef } else {0} };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{>>Rejecting production<< (found <reject>)},
					 Parse::RecDescent::_tracefirst($text),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		undef $return;
		

		$_tok = undef;
		
		last unless defined $_tok;

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [<error?:...> <reject>]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{facts},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{facts},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{facts},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::atom
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"atom"};
	
	Parse::RecDescent::_trace(q{Trying rule: [atom]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{atom},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [predicate]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{atom},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{atom});
		%item = (__RULE__ => q{atom});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [predicate]},
				  Parse::RecDescent::_tracefirst($text),
				  q{atom},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::predicate($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [predicate]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{atom},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [predicate]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{atom},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{predicate}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [predicate]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{atom},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [variable]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{atom},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{atom});
		%item = (__RULE__ => q{atom});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [variable]},
				  Parse::RecDescent::_tracefirst($text),
				  q{atom},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::variable($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [variable]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{atom},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [variable]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{atom},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{variable}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [variable]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{atom},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [literal]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{atom},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[2];
		$text = $_[1];
		my $_savetext;
		@item = (q{atom});
		%item = (__RULE__ => q{atom});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [literal]},
				  Parse::RecDescent::_tracefirst($text),
				  q{atom},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::literal($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [literal]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{atom},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [literal]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{atom},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{literal}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [literal]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{atom},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{atom},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{atom},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{atom},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{atom},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::new_facts
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"new_facts"};
	
	Parse::RecDescent::_trace(q{Trying rule: [new_facts]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{new_facts},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [clause ',' <commit> new_facts]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{new_facts});
		%item = (__RULE__ => q{new_facts});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [clause]},
				  Parse::RecDescent::_tracefirst($text),
				  q{new_facts},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::clause($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [clause]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{new_facts},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [clause]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{clause}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [',']},
					  Parse::RecDescent::_tracefirst($text),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{','})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\,//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<commit>]},
					Parse::RecDescent::_tracefirst($text),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { $commit = 1 };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [new_facts]},
				  Parse::RecDescent::_tracefirst($text),
				  q{new_facts},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{new_facts})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::new_facts($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [new_facts]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{new_facts},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [new_facts]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{new_facts}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { "    (assert $item{clause})\n" . $item{new_facts} };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [clause ',' <commit> new_facts]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [clause]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{new_facts});
		%item = (__RULE__ => q{new_facts});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [clause]},
				  Parse::RecDescent::_tracefirst($text),
				  q{new_facts},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::clause($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [clause]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{new_facts},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [clause]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{clause}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { "    (assert $item{clause})\n" };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [clause]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [<error?:...> <reject>]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[2];
		
		my $_savetext;
		@item = (q{new_facts});
		%item = (__RULE__ => q{new_facts});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<error?:...>]},
					Parse::RecDescent::_tracefirst($text),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { if ($commit) { do {
		my $rule = $item[0];
		   $rule =~ s/_/ /g;
		#WAS: Parse::RecDescent::_error("Invalid $rule: " . $expectation->message() ,$thisline);
		push @{$thisparser->{errors}}, ["Invalid $rule: " . $expectation->message() ,$thisline];
		} unless  $_noactions; undef } else {0} };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{>>Rejecting production<< (found <reject>)},
					 Parse::RecDescent::_tracefirst($text),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		undef $return;
		

		$_tok = undef;
		
		last unless defined $_tok;

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [<error?:...> <reject>]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{new_facts},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{new_facts},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{new_facts},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{new_facts},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::eofile
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"eofile"};
	
	Parse::RecDescent::_trace(q{Trying rule: [eofile]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{eofile},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [/^\\Z/]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{eofile},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{eofile});
		%item = (__RULE__ => q{eofile});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/^\\Z/]}, Parse::RecDescent::_tracefirst($text),
					  q{eofile},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:^\Z)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [/^\\Z/]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{eofile},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{eofile},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{eofile},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{eofile},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{eofile},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::infix
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"infix"};
	
	Parse::RecDescent::_trace(q{Trying rule: [infix]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{infix},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [variable]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{infix});
		%item = (__RULE__ => q{infix});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [variable]},
				  Parse::RecDescent::_tracefirst($text),
				  q{infix},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::variable($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [variable]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{infix},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [variable]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{variable}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [variable]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: []},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{infix});
		%item = (__RULE__ => q{infix});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { ::match_infix($text) };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { $::infix{$item[1]} };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION2__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: []<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{infix},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{infix},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{infix},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::infix_prefix
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"infix_prefix"};
	
	Parse::RecDescent::_trace(q{Trying rule: [infix_prefix]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{infix_prefix},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: ['@']},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{infix_prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{infix_prefix});
		%item = (__RULE__ => q{infix_prefix});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['@']},
					  Parse::RecDescent::_tracefirst($text),
					  q{infix_prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\@//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{infix_prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { 'vector-relation' };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: ['@']<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{infix_prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: ['%']},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{infix_prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{infix_prefix});
		%item = (__RULE__ => q{infix_prefix});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['%']},
					  Parse::RecDescent::_tracefirst($text),
					  q{infix_prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\%//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{infix_prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { 'space-relation' };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: ['%']<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{infix_prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{infix_prefix},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{infix_prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{infix_prefix},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{infix_prefix},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::conjunction
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"conjunction"};
	
	Parse::RecDescent::_trace(q{Trying rule: [conjunction]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{conjunction},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [clause ',' <commit> conjunction]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{conjunction});
		%item = (__RULE__ => q{conjunction});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [clause]},
				  Parse::RecDescent::_tracefirst($text),
				  q{conjunction},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::clause($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [clause]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{conjunction},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [clause]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{clause}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [',']},
					  Parse::RecDescent::_tracefirst($text),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{','})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\,//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<commit>]},
					Parse::RecDescent::_tracefirst($text),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { $commit = 1 };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [conjunction]},
				  Parse::RecDescent::_tracefirst($text),
				  q{conjunction},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{conjunction})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::conjunction($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [conjunction]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{conjunction},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [conjunction]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{conjunction}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { "(and $item{clause} $item{conjunction})" };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [clause ',' <commit> conjunction]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [clause]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{conjunction});
		%item = (__RULE__ => q{conjunction});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [clause]},
				  Parse::RecDescent::_tracefirst($text),
				  q{conjunction},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::clause($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [clause]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{conjunction},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [clause]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{clause}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [clause]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [<error?:...> <reject>]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[2];
		
		my $_savetext;
		@item = (q{conjunction});
		%item = (__RULE__ => q{conjunction});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<error?:...>]},
					Parse::RecDescent::_tracefirst($text),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { if ($commit) { do {
		my $rule = $item[0];
		   $rule =~ s/_/ /g;
		#WAS: Parse::RecDescent::_error("Invalid $rule: " . $expectation->message() ,$thisline);
		push @{$thisparser->{errors}}, ["Invalid $rule: " . $expectation->message() ,$thisline];
		} unless  $_noactions; undef } else {0} };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{>>Rejecting production<< (found <reject>)},
					 Parse::RecDescent::_tracefirst($text),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		undef $return;
		

		$_tok = undef;
		
		last unless defined $_tok;

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [<error?:...> <reject>]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{conjunction},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{conjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{conjunction},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{conjunction},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::compound
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"compound"};
	
	Parse::RecDescent::_trace(q{Trying rule: [compound]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{compound},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [<leftop: clause /([;,])/ clause>]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{compound},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{compound});
		%item = (__RULE__ => q{compound});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying operator: [<leftop: clause /([;,])/ clause>]},
				  Parse::RecDescent::_tracefirst($text),
				  q{compound},
				  $tracelevel)
					if defined $::RD_TRACE;
		$expectation->is(q{})->at($text);

		$_tok = undef;
		OPLOOP: while (1)
		{
		  $repcount = 0;
		  my  @item;
		  
		  # MATCH LEFTARG
		  
		Parse::RecDescent::_trace(q{Trying subrule: [clause]},
				  Parse::RecDescent::_tracefirst($text),
				  q{compound},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{clause})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::clause($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [clause]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{compound},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [clause]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{compound},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{clause}} = $_tok;
		push @item, $_tok;
		
		}


		  $repcount++;

		  my $savetext = $text;
		  my $backtrack;

		  # MATCH (OP RIGHTARG)(s)
		  while ($repcount < 100000000)
		  {
			$backtrack = 0;
			
		Parse::RecDescent::_trace(q{Trying terminal: [/([;,])/]}, Parse::RecDescent::_tracefirst($text),
					  q{compound},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{/([;,])/})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and   $text =~ s/\A(?:([;,]))//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

			pop @item;
			if (defined $1) {push @item, $item{__DIRECTIVE1__}=$1; $backtrack=1;}
			
		Parse::RecDescent::_trace(q{Trying subrule: [clause]},
				  Parse::RecDescent::_tracefirst($text),
				  q{compound},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{clause})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::clause($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [clause]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{compound},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [clause]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{compound},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{clause}} = $_tok;
		push @item, $_tok;
		
		}

			$savetext = $text;
			$repcount++;
		  }
		  $text = $savetext;
		  pop @item if $backtrack;

		  unless (@item) { undef $_tok; last }
		  $_tok = [ @item ];
		  last;
		} 

		unless ($repcount>=1)
		{
			Parse::RecDescent::_trace(q{<<Didn't match operator: [<leftop: clause /([;,])/ clause>]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{compound},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched operator: [<leftop: clause /([;,])/ clause>]<< (return value: [}
					  . qq{@{$_tok||[]}} . q{]},
					  Parse::RecDescent::_tracefirst($text),
					  q{compound},
					  $tracelevel)
						if defined $::RD_TRACE;

		push @item, $item{__DIRECTIVE1__}=$_tok||[];


		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{compound},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { "\n    " . join "", (map { m/^[;,]$/ ? "$_\n    " : $_ } @{ $item[1] }); };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [<leftop: clause /([;,])/ clause>]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{compound},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{compound},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{compound},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{compound},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{compound},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::disjunction
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"disjunction"};
	
	Parse::RecDescent::_trace(q{Trying rule: [disjunction]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{disjunction},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [conjunction ';' <commit> disjunction]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{disjunction});
		%item = (__RULE__ => q{disjunction});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [conjunction]},
				  Parse::RecDescent::_tracefirst($text),
				  q{disjunction},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::conjunction($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [conjunction]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{disjunction},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [conjunction]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{conjunction}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [';']},
					  Parse::RecDescent::_tracefirst($text),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{';'})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\;//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<commit>]},
					Parse::RecDescent::_tracefirst($text),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { $commit = 1 };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [disjunction]},
				  Parse::RecDescent::_tracefirst($text),
				  q{disjunction},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{disjunction})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::disjunction($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [disjunction]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{disjunction},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [disjunction]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{disjunction}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { "(or $item{conjunction} $item{disjunction})" };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [conjunction ';' <commit> disjunction]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [conjunction]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{disjunction});
		%item = (__RULE__ => q{disjunction});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [conjunction]},
				  Parse::RecDescent::_tracefirst($text),
				  q{disjunction},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::conjunction($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [conjunction]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{disjunction},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [conjunction]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{conjunction}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [conjunction]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [<error?:...> <reject>]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[2];
		
		my $_savetext;
		@item = (q{disjunction});
		%item = (__RULE__ => q{disjunction});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<error?:...>]},
					Parse::RecDescent::_tracefirst($text),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { if ($commit) { do {
		my $rule = $item[0];
		   $rule =~ s/_/ /g;
		#WAS: Parse::RecDescent::_error("Invalid $rule: " . $expectation->message() ,$thisline);
		push @{$thisparser->{errors}}, ["Invalid $rule: " . $expectation->message() ,$thisline];
		} unless  $_noactions; undef } else {0} };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{>>Rejecting production<< (found <reject>)},
					 Parse::RecDescent::_tracefirst($text),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		undef $return;
		

		$_tok = undef;
		
		last unless defined $_tok;

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [<error?:...> <reject>]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{disjunction},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{disjunction},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{disjunction},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{disjunction},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::general_infix
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"general_infix"};
	
	Parse::RecDescent::_trace(q{Trying rule: [general_infix]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{general_infix},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [infix_prefix <commit> infix]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{general_infix});
		%item = (__RULE__ => q{general_infix});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [infix_prefix]},
				  Parse::RecDescent::_tracefirst($text),
				  q{general_infix},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::infix_prefix($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [infix_prefix]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{general_infix},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [infix_prefix]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{infix_prefix}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<commit>]},
					Parse::RecDescent::_tracefirst($text),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { $commit = 1 };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [infix]},
				  Parse::RecDescent::_tracefirst($text),
				  q{general_infix},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{infix})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::infix($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [infix]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{general_infix},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [infix]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{infix}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { "$item{infix_prefix} $item{infix}" };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [infix_prefix <commit> infix]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [infix]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{general_infix});
		%item = (__RULE__ => q{general_infix});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [infix]},
				  Parse::RecDescent::_tracefirst($text),
				  q{general_infix},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::infix($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [infix]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{general_infix},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [infix]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{infix}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [infix]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [<error?:...> <reject>]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[2];
		
		my $_savetext;
		@item = (q{general_infix});
		%item = (__RULE__ => q{general_infix});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<error?:...>]},
					Parse::RecDescent::_tracefirst($text),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { if ($commit) { do {
		my $rule = $item[0];
		   $rule =~ s/_/ /g;
		#WAS: Parse::RecDescent::_error("Invalid $rule: " . $expectation->message() ,$thisline);
		push @{$thisparser->{errors}}, ["Invalid $rule: " . $expectation->message() ,$thisline];
		} unless  $_noactions; undef } else {0} };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{>>Rejecting production<< (found <reject>)},
					 Parse::RecDescent::_tracefirst($text),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		undef $return;
		

		$_tok = undef;
		
		last unless defined $_tok;

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [<error?:...> <reject>]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{general_infix},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{general_infix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{general_infix},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{general_infix},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::pattern
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"pattern"};
	
	Parse::RecDescent::_trace(q{Trying rule: [pattern]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{pattern},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [/\\S+(?=>)/]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{pattern},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{pattern});
		%item = (__RULE__ => q{pattern});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/\\S+(?=>)/]}, Parse::RecDescent::_tracefirst($text),
					  q{pattern},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:\S+(?=>))//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [/\\S+(?=>)/]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{pattern},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{pattern},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{pattern},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{pattern},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{pattern},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::comment
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"comment"};
	
	Parse::RecDescent::_trace(q{Trying rule: [comment]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{comment},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [/\\/\\*(.*?)\\*\\//s]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{comment},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{comment});
		%item = (__RULE__ => q{comment});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/\\/\\*(.*?)\\*\\//s]}, Parse::RecDescent::_tracefirst($text),
					  q{comment},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:\/\*(.*?)\*\/)//s)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{comment},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { my $cmt = "; $1"; $cmt =~ s/\n(?=.)/;/g; $cmt };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [/\\/\\*(.*?)\\*\\//s]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{comment},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{comment},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{comment},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{comment},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{comment},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::clause
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"clause"};
	
	Parse::RecDescent::_trace(q{Trying rule: [clause]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{clause},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		local $skip = defined($skip) ? $skip : $Parse::RecDescent::skip;
		Parse::RecDescent::_trace(q{Trying production: [prefix <skip:''> <commit> atom]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{clause});
		%item = (__RULE__ => q{clause});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [prefix]},
				  Parse::RecDescent::_tracefirst($text),
				  q{clause},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::prefix($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [prefix]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{clause},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [prefix]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{prefix}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<skip:''>]},
					Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { my $oldskip = $skip; $skip=''; $oldskip };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<commit>]},
					Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { $commit = 1 };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE2__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [atom]},
				  Parse::RecDescent::_tracefirst($text),
				  q{clause},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{atom})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::atom($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [atom]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{clause},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [atom]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{atom}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { "($item{prefix} $item{atom})" };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [prefix <skip:''> <commit> atom]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		local $skip = defined($skip) ? $skip : $Parse::RecDescent::skip;
		Parse::RecDescent::_trace(q{Trying production: [atom <skip:''> /\\s+/ general_infix <skip:'\s*'> atom]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{clause});
		%item = (__RULE__ => q{clause});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [atom]},
				  Parse::RecDescent::_tracefirst($text),
				  q{clause},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::atom($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [atom]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{clause},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [atom]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{atom}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<skip:''>]},
					Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { my $oldskip = $skip; $skip=''; $oldskip };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/\\s+/]}, Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{/\\s+/})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:\s+)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [general_infix]},
				  Parse::RecDescent::_tracefirst($text),
				  q{clause},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{general_infix})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::general_infix($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [general_infix]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{clause},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [general_infix]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{general_infix}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<skip:'\s*'>]},
					Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { my $oldskip = $skip; $skip='\s*'; $oldskip };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE2__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [atom]},
				  Parse::RecDescent::_tracefirst($text),
				  q{clause},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{atom})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::atom($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [atom]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{clause},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [atom]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{atom}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { "($item{general_infix} $item[1] $item[6])" };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [atom <skip:''> /\\s+/ general_infix <skip:'\s*'> atom]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		local $skip = defined($skip) ? $skip : $Parse::RecDescent::skip;
		Parse::RecDescent::_trace(q{Trying production: [atom <skip:''> postfix]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[2];
		$text = $_[1];
		my $_savetext;
		@item = (q{clause});
		%item = (__RULE__ => q{clause});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [atom]},
				  Parse::RecDescent::_tracefirst($text),
				  q{clause},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::atom($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [atom]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{clause},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [atom]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{atom}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<skip:''>]},
					Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { my $oldskip = $skip; $skip=''; $oldskip };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [postfix]},
				  Parse::RecDescent::_tracefirst($text),
				  q{clause},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{postfix})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::postfix($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [postfix]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{clause},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [postfix]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{postfix}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { "($item{postfix} $item{atom})" };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [atom <skip:''> postfix]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [atom]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[3];
		$text = $_[1];
		my $_savetext;
		@item = (q{clause});
		%item = (__RULE__ => q{clause});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [atom]},
				  Parse::RecDescent::_tracefirst($text),
				  q{clause},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::atom($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [atom]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{clause},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [atom]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{atom}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [atom]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [<error?:...> <reject>]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[4];
		
		my $_savetext;
		@item = (q{clause});
		%item = (__RULE__ => q{clause});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<error?:...>]},
					Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { if ($commit) { do {
		my $rule = $item[0];
		   $rule =~ s/_/ /g;
		#WAS: Parse::RecDescent::_error("Invalid $rule: " . $expectation->message() ,$thisline);
		push @{$thisparser->{errors}}, ["Invalid $rule: " . $expectation->message() ,$thisline];
		} unless  $_noactions; undef } else {0} };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{>>Rejecting production<< (found <reject>)},
					 Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		undef $return;
		

		$_tok = undef;
		
		last unless defined $_tok;

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [<error?:...> <reject>]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{clause},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{clause},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{clause},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{clause},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::arguments
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"arguments"};
	
	Parse::RecDescent::_trace(q{Trying rule: [arguments]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{arguments},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [<leftop: clause ',' clause>]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{arguments},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{arguments});
		%item = (__RULE__ => q{arguments});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying operator: [<leftop: clause ',' clause>]},
				  Parse::RecDescent::_tracefirst($text),
				  q{arguments},
				  $tracelevel)
					if defined $::RD_TRACE;
		$expectation->is(q{})->at($text);

		$_tok = undef;
		OPLOOP: while (1)
		{
		  $repcount = 0;
		  my  @item;
		  
		  # MATCH LEFTARG
		  
		Parse::RecDescent::_trace(q{Trying subrule: [clause]},
				  Parse::RecDescent::_tracefirst($text),
				  q{arguments},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{clause})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::clause($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [clause]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{arguments},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [clause]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{arguments},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{clause}} = $_tok;
		push @item, $_tok;
		
		}


		  $repcount++;

		  my $savetext = $text;
		  my $backtrack;

		  # MATCH (OP RIGHTARG)(s)
		  while ($repcount < 100000000)
		  {
			$backtrack = 0;
			
		Parse::RecDescent::_trace(q{Trying terminal: [',']},
					  Parse::RecDescent::_tracefirst($text),
					  q{arguments},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{','})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and   $text =~ s/\A\,//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

			pop @item;
			
			
		Parse::RecDescent::_trace(q{Trying subrule: [clause]},
				  Parse::RecDescent::_tracefirst($text),
				  q{arguments},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{clause})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::clause($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [clause]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{arguments},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [clause]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{arguments},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{clause}} = $_tok;
		push @item, $_tok;
		
		}

			$savetext = $text;
			$repcount++;
		  }
		  $text = $savetext;
		  pop @item if $backtrack;

		  unless (@item) { undef $_tok; last }
		  $_tok = [ @item ];
		  last;
		} 

		unless ($repcount>=1)
		{
			Parse::RecDescent::_trace(q{<<Didn't match operator: [<leftop: clause ',' clause>]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{arguments},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched operator: [<leftop: clause ',' clause>]<< (return value: [}
					  . qq{@{$_tok||[]}} . q{]},
					  Parse::RecDescent::_tracefirst($text),
					  q{arguments},
					  $tracelevel)
						if defined $::RD_TRACE;

		push @item, $item{__DIRECTIVE1__}=$_tok||[];


		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{arguments},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { join ' ', @{ $item[1] } };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [<leftop: clause ',' clause>]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{arguments},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{arguments},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{arguments},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{arguments},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{arguments},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::directive
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"directive"};
	
	Parse::RecDescent::_trace(q{Trying rule: [directive]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{directive},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: ['module' <commit> identifier '.' /[\\n\\s]*/]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{directive});
		%item = (__RULE__ => q{directive});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['module']},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\Amodule//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<commit>]},
					Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { $commit = 1 };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [identifier]},
				  Parse::RecDescent::_tracefirst($text),
				  q{directive},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{identifier})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::identifier($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [identifier]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{directive},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [identifier]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{identifier}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['.']},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{'.'})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\.//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING2__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/[\\n\\s]*/]}, Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{/[\\n\\s]*/})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:[\n\s]*)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { $::module = uc($item{identifier}) . '::'; '' };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: ['module' <commit> identifier '.' /[\\n\\s]*/]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: ['include' <commit> string '.' /[\\n\\s]*/]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[1];
		$text = $_[1];
		my $_savetext;
		@item = (q{directive});
		%item = (__RULE__ => q{directive});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['include']},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\Ainclude//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<commit>]},
					Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { $commit = 1 };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [string]},
				  Parse::RecDescent::_tracefirst($text),
				  q{directive},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{string})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::string($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [string]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{directive},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [string]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{string}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['.']},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{'.'})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\.//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING2__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/[\\n\\s]*/]}, Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{/[\\n\\s]*/})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:[\n\s]*)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { my $res = ::process_include(eval $item{string}, $itempos[1]{line}{from}) };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: ['include' <commit> string '.' /[\\n\\s]*/]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		local $skip = defined($skip) ? $skip : $Parse::RecDescent::skip;
		Parse::RecDescent::_trace(q{Trying production: ['prefix:<' <commit> <skip:''> pattern '>' <skip:'\s*'> string '.' /[\\n\\s]*/]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[2];
		$text = $_[1];
		my $_savetext;
		@item = (q{directive});
		%item = (__RULE__ => q{directive});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['prefix:<']},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\Aprefix\:\<//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<commit>]},
					Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { $commit = 1 };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<skip:''>]},
					Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { my $oldskip = $skip; $skip=''; $oldskip };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE2__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [pattern]},
				  Parse::RecDescent::_tracefirst($text),
				  q{directive},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{pattern})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::pattern($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [pattern]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{directive},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [pattern]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{pattern}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['>']},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{'>'})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\>//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING2__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<skip:'\s*'>]},
					Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { my $oldskip = $skip; $skip='\s*'; $oldskip };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE3__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [string]},
				  Parse::RecDescent::_tracefirst($text),
				  q{directive},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{string})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::string($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [string]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{directive},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [string]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{string}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['.']},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{'.'})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\.//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING3__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/[\\n\\s]*/]}, Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{/[\\n\\s]*/})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:[\n\s]*)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { $::prefix{$item{pattern}} = eval $item{string}; '' };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: ['prefix:<' <commit> <skip:''> pattern '>' <skip:'\s*'> string '.' /[\\n\\s]*/]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched && !$commit)
	{
		local $skip = defined($skip) ? $skip : $Parse::RecDescent::skip;
		Parse::RecDescent::_trace(q{Trying production: ['infix:<' <commit> <skip:''> pattern '>' <skip:'\s*'> string '.' /[\\n\\s]*/]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[3];
		$text = $_[1];
		my $_savetext;
		@item = (q{directive});
		%item = (__RULE__ => q{directive});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['infix:<']},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\Ainfix\:\<//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<commit>]},
					Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { $commit = 1 };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<skip:''>]},
					Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { my $oldskip = $skip; $skip=''; $oldskip };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE2__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [pattern]},
				  Parse::RecDescent::_tracefirst($text),
				  q{directive},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{pattern})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::pattern($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [pattern]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{directive},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [pattern]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{pattern}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['>']},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{'>'})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\>//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING2__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<skip:'\s*'>]},
					Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { my $oldskip = $skip; $skip='\s*'; $oldskip };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE3__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying subrule: [string]},
				  Parse::RecDescent::_tracefirst($text),
				  q{directive},
				  $tracelevel)
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(q{string})->at($text);
		unless (defined ($_tok = Parse::RecDescent::CLIPSx::string($thisparser,$text,$repeating,$_noactions,sub { \@arg })))
		{
			
			Parse::RecDescent::_trace(q{<<Didn't match subrule: [string]>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{directive},
						  $tracelevel)
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: [string]<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$item{q{string}} = $_tok;
		push @item, $_tok;
		
		}

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: ['.']},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{'.'})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A\.//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $item{__STRING3__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying terminal: [/[\\n\\s]*/]}, Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{/[\\n\\s]*/})->at($text);
		

		unless ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and do {
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	 1} and   $text =~ s/\A(?:[\n\s]*)//)
		{
			
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn't match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $item{__PATTERN1__}=$&;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { $::infix{$item{pattern}} = eval $item{string}; '' };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: ['infix:<' <commit> <skip:''> pattern '>' <skip:'\s*'> string '.' /[\\n\\s]*/]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


	while (!$_matched)
	{
		
		Parse::RecDescent::_trace(q{Trying production: [<error?:...> <reject>]},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[4];
		
		my $_savetext;
		@item = (q{directive});
		%item = (__RULE__ => q{directive});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		

		Parse::RecDescent::_trace(q{Trying directive: [<error?:...>]},
					Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE; 
		$_tok = do { if ($commit) { do {
		my $rule = $item[0];
		   $rule =~ s/_/ /g;
		#WAS: Parse::RecDescent::_error("Invalid $rule: " . $expectation->message() ,$thisline);
		push @{$thisparser->{errors}}, ["Invalid $rule: " . $expectation->message() ,$thisline];
		} unless  $_noactions; undef } else {0} };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn't match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		
		last unless defined $_tok;
		push @item, $item{__DIRECTIVE1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{>>Rejecting production<< (found <reject>)},
					 Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		undef $return;
		

		$_tok = undef;
		
		last unless defined $_tok;

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: [<error?:...> <reject>]<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{directive},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{directive},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{directive},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{directive},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}

# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub Parse::RecDescent::CLIPSx::prefix
{
	my $thisparser = $_[0];
	use vars q{$tracelevel};
	local $tracelevel = ($tracelevel||0)+1;
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"prefix"};
	
	Parse::RecDescent::_trace(q{Trying rule: [prefix]},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{prefix},
				  $tracelevel)
					if defined $::RD_TRACE;

	
	my $err_at = @{$thisparser->{errors}};

	my $score;
	my $score_return;
	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my %item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);
	
	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	
	my $prevoffset;
	tie $prevoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser, 1;
	
	my $thiscolumn;
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	
	my $prevcolumn;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	
	my $prevline;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;
	
	my $thisline;
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;

	

	while (!$_matched && !$commit)
	{
		
		Parse::RecDescent::_trace(q{Trying production: []},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[0];
		$text = $_[1];
		my $_savetext;
		@item = (q{prefix});
		%item = (__RULE__ => q{prefix});
		my $repcount = 0;

		my @itempos = ({});

		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { ::match_prefix($text) };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION1__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		

		$_tok = ($_noactions) ? 0 : do { $::prefix{$item[1]} };
		unless (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn't match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])},
					  Parse::RecDescent::_tracefirst($text))
						if defined $::RD_TRACE;
		push @item, $_tok;
		$item{__ACTION2__}=$_tok;
		

		$itempos[$#itempos]{'offset'}{'to'} = $prevoffset;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	

		Parse::RecDescent::_trace(q{>>Matched production: []<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}


        unless ( $_matched || defined($return) || defined($score) )
	{
		

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn't match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{prefix},
					 $tracelevel)
					if defined $::RD_TRACE;
		return undef;
	}
	if (!defined($return) && defined($score))
	{
		Parse::RecDescent::_trace(q{>>Accepted scored production<<}, "",
					  q{prefix},
					  $tracelevel)
						if defined $::RD_TRACE;
		$return = $score_return;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{prefix},
					  $tracelevel);
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{prefix},
					  $tracelevel)
	}
	$_[1] = $text;
	return $return;
}
}
package CLIPSx; sub new { my $self = bless( {
                 '_AUTOTREE' => undef,
                 'localvars' => '',
                 'startcode' => '',
                 '_check' => {
                               'thisoffset' => 1,
                               'itempos' => 1,
                               'prevoffset' => 1,
                               'prevline' => 1,
                               'prevcolumn' => 1,
                               'thiscolumn' => 1
                             },
                 'namespace' => 'Parse::RecDescent::CLIPSx',
                 '_AUTOACTION' => undef,
                 'rules' => {
                              'number' => bless( {
                                                   'impcount' => 0,
                                                   'calls' => [],
                                                   'changed' => 0,
                                                   'opcount' => 0,
                                                   'prods' => [
                                                                bless( {
                                                                         'number' => '0',
                                                                         'strcount' => 0,
                                                                         'dircount' => 0,
                                                                         'uncommit' => undef,
                                                                         'error' => undef,
                                                                         'patcount' => 1,
                                                                         'actcount' => 0,
                                                                         'items' => [
                                                                                      bless( {
                                                                                               'pattern' => '\\d+(?:\\.\\d*)?',
                                                                                               'hashname' => '__PATTERN1__',
                                                                                               'description' => '/\\\\d+(?:\\\\.\\\\d*)?/',
                                                                                               'lookahead' => 0,
                                                                                               'rdelim' => '/',
                                                                                               'line' => 115,
                                                                                               'mod' => '',
                                                                                               'ldelim' => '/'
                                                                                             }, 'Parse::RecDescent::Token' )
                                                                                    ],
                                                                         'line' => undef
                                                                       }, 'Parse::RecDescent::Production' ),
                                                                bless( {
                                                                         'number' => '1',
                                                                         'strcount' => 0,
                                                                         'dircount' => 0,
                                                                         'uncommit' => undef,
                                                                         'error' => undef,
                                                                         'patcount' => 1,
                                                                         'actcount' => 0,
                                                                         'items' => [
                                                                                      bless( {
                                                                                               'pattern' => '\\.\\d+',
                                                                                               'hashname' => '__PATTERN1__',
                                                                                               'description' => '/\\\\.\\\\d+/',
                                                                                               'lookahead' => 0,
                                                                                               'rdelim' => '/',
                                                                                               'line' => 116,
                                                                                               'mod' => '',
                                                                                               'ldelim' => '/'
                                                                                             }, 'Parse::RecDescent::Token' )
                                                                                    ],
                                                                         'line' => 116
                                                                       }, 'Parse::RecDescent::Production' )
                                                              ],
                                                   'name' => 'number',
                                                   'vars' => '',
                                                   'line' => 115
                                                 }, 'Parse::RecDescent::Rule' ),
                              'variable' => bless( {
                                                     'impcount' => 0,
                                                     'calls' => [],
                                                     'changed' => 0,
                                                     'opcount' => 0,
                                                     'prods' => [
                                                                  bless( {
                                                                           'number' => '0',
                                                                           'strcount' => 0,
                                                                           'dircount' => 0,
                                                                           'uncommit' => undef,
                                                                           'error' => undef,
                                                                           'patcount' => 1,
                                                                           'actcount' => 1,
                                                                           'items' => [
                                                                                        bless( {
                                                                                                 'pattern' => '\\?[A-Za-z_]([-\\w])*',
                                                                                                 'hashname' => '__PATTERN1__',
                                                                                                 'description' => '/\\\\?[A-Za-z_]([-\\\\w])*/',
                                                                                                 'lookahead' => 0,
                                                                                                 'rdelim' => '/',
                                                                                                 'line' => 106,
                                                                                                 'mod' => '',
                                                                                                 'ldelim' => '/'
                                                                                               }, 'Parse::RecDescent::Token' ),
                                                                                        bless( {
                                                                                                 'hashname' => '__ACTION1__',
                                                                                                 'lookahead' => 0,
                                                                                                 'line' => 106,
                                                                                                 'code' => '{ $item[1] . $item{identifier} }'
                                                                                               }, 'Parse::RecDescent::Action' )
                                                                                      ],
                                                                           'line' => undef
                                                                         }, 'Parse::RecDescent::Production' ),
                                                                  bless( {
                                                                           'number' => '1',
                                                                           'strcount' => 1,
                                                                           'dircount' => 0,
                                                                           'uncommit' => undef,
                                                                           'error' => undef,
                                                                           'patcount' => 0,
                                                                           'actcount' => 0,
                                                                           'items' => [
                                                                                        bless( {
                                                                                                 'pattern' => '?',
                                                                                                 'hashname' => '__STRING1__',
                                                                                                 'description' => '\'?\'',
                                                                                                 'lookahead' => 0,
                                                                                                 'line' => 107
                                                                                               }, 'Parse::RecDescent::Literal' )
                                                                                      ],
                                                                           'line' => 107
                                                                         }, 'Parse::RecDescent::Production' )
                                                                ],
                                                     'name' => 'variable',
                                                     'vars' => '',
                                                     'line' => 106
                                                   }, 'Parse::RecDescent::Rule' ),
                              'fact' => bless( {
                                                 'impcount' => 0,
                                                 'calls' => [
                                                              'clause'
                                                            ],
                                                 'changed' => 0,
                                                 'opcount' => 0,
                                                 'prods' => [
                                                              bless( {
                                                                       'number' => '0',
                                                                       'strcount' => 0,
                                                                       'dircount' => 0,
                                                                       'uncommit' => undef,
                                                                       'error' => undef,
                                                                       'patcount' => 0,
                                                                       'actcount' => 1,
                                                                       'items' => [
                                                                                    bless( {
                                                                                             'subrule' => 'clause',
                                                                                             'matchrule' => 0,
                                                                                             'implicit' => undef,
                                                                                             'argcode' => undef,
                                                                                             'lookahead' => 0,
                                                                                             'line' => 66
                                                                                           }, 'Parse::RecDescent::Subrule' ),
                                                                                    bless( {
                                                                                             'hashname' => '__ACTION1__',
                                                                                             'lookahead' => 0,
                                                                                             'line' => 66,
                                                                                             'code' => '{ push @::facts, "; $::infile (line $itempos[1]{line}{from} ~ ".
                    "$itempos[1]{line}{to})",
                    "$item{clause}\\n"; \'\' }'
                                                                                           }, 'Parse::RecDescent::Action' )
                                                                                  ],
                                                                       'line' => undef
                                                                     }, 'Parse::RecDescent::Production' )
                                                            ],
                                                 'name' => 'fact',
                                                 'vars' => '',
                                                 'line' => 66
                                               }, 'Parse::RecDescent::Rule' ),
                              'string' => bless( {
                                                   'impcount' => 0,
                                                   'calls' => [],
                                                   'changed' => 0,
                                                   'opcount' => 0,
                                                   'prods' => [
                                                                bless( {
                                                                         'number' => '0',
                                                                         'strcount' => 0,
                                                                         'dircount' => 0,
                                                                         'uncommit' => undef,
                                                                         'error' => undef,
                                                                         'patcount' => 0,
                                                                         'actcount' => 1,
                                                                         'items' => [
                                                                                      bless( {
                                                                                               'hashname' => '__ACTION1__',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 118,
                                                                                               'code' => '{ extract_delimited($text, \'"\') }'
                                                                                             }, 'Parse::RecDescent::Action' )
                                                                                    ],
                                                                         'line' => undef
                                                                       }, 'Parse::RecDescent::Production' )
                                                              ],
                                                   'name' => 'string',
                                                   'vars' => '',
                                                   'line' => 118
                                                 }, 'Parse::RecDescent::Rule' ),
                              'rule' => bless( {
                                                 'impcount' => 0,
                                                 'calls' => [
                                                              'disjunction',
                                                              'new_facts'
                                                            ],
                                                 'changed' => 0,
                                                 'opcount' => 0,
                                                 'prods' => [
                                                              bless( {
                                                                       'number' => '0',
                                                                       'strcount' => 2,
                                                                       'dircount' => 1,
                                                                       'uncommit' => undef,
                                                                       'error' => undef,
                                                                       'patcount' => 1,
                                                                       'actcount' => 1,
                                                                       'items' => [
                                                                                    bless( {
                                                                                             'subrule' => 'disjunction',
                                                                                             'matchrule' => 0,
                                                                                             'implicit' => undef,
                                                                                             'argcode' => undef,
                                                                                             'lookahead' => 0,
                                                                                             'line' => 6
                                                                                           }, 'Parse::RecDescent::Subrule' ),
                                                                                    bless( {
                                                                                             'pattern' => '=>',
                                                                                             'hashname' => '__STRING1__',
                                                                                             'description' => '\'=>\'',
                                                                                             'lookahead' => 0,
                                                                                             'line' => 6
                                                                                           }, 'Parse::RecDescent::Literal' ),
                                                                                    bless( {
                                                                                             'hashname' => '__DIRECTIVE1__',
                                                                                             'name' => '<commit>',
                                                                                             'lookahead' => 0,
                                                                                             'line' => 6,
                                                                                             'code' => '$commit = 1'
                                                                                           }, 'Parse::RecDescent::Directive' ),
                                                                                    bless( {
                                                                                             'subrule' => 'new_facts',
                                                                                             'matchrule' => 0,
                                                                                             'implicit' => undef,
                                                                                             'argcode' => undef,
                                                                                             'lookahead' => 0,
                                                                                             'line' => 6
                                                                                           }, 'Parse::RecDescent::Subrule' ),
                                                                                    bless( {
                                                                                             'pattern' => '.',
                                                                                             'hashname' => '__STRING2__',
                                                                                             'description' => '\'.\'',
                                                                                             'lookahead' => 0,
                                                                                             'line' => 6
                                                                                           }, 'Parse::RecDescent::Literal' ),
                                                                                    bless( {
                                                                                             'pattern' => '[\\n\\s]*',
                                                                                             'hashname' => '__PATTERN1__',
                                                                                             'description' => '/[\\\\n\\\\s]*/',
                                                                                             'lookahead' => 0,
                                                                                             'rdelim' => '/',
                                                                                             'line' => 6,
                                                                                             'mod' => '',
                                                                                             'ldelim' => '/'
                                                                                           }, 'Parse::RecDescent::Token' ),
                                                                                    bless( {
                                                                                             'hashname' => '__ACTION1__',
                                                                                             'lookahead' => 0,
                                                                                             'line' => 8,
                                                                                             'code' => '{ $::count++;
          "; $::infile (line $itempos[1]{line}{from} ~ $itempos[5]{line}{to})\\n".
          "(defrule $::module$::base-$::count\\n".
          "    $item[1]\\n".
          "    =>\\n".
          "$item[4])" }'
                                                                                           }, 'Parse::RecDescent::Action' )
                                                                                  ],
                                                                       'line' => undef
                                                                     }, 'Parse::RecDescent::Production' ),
                                                              bless( {
                                                                       'number' => '1',
                                                                       'strcount' => 0,
                                                                       'dircount' => 2,
                                                                       'uncommit' => 0,
                                                                       'error' => 1,
                                                                       'patcount' => 0,
                                                                       'actcount' => 0,
                                                                       'items' => [
                                                                                    bless( {
                                                                                             'msg' => '',
                                                                                             'hashname' => '__DIRECTIVE1__',
                                                                                             'commitonly' => '?',
                                                                                             'lookahead' => 0,
                                                                                             'line' => 15
                                                                                           }, 'Parse::RecDescent::Error' ),
                                                                                    bless( {
                                                                                             'hashname' => '__DIRECTIVE2__',
                                                                                             'name' => '<reject>',
                                                                                             'lookahead' => 0,
                                                                                             'line' => 15
                                                                                           }, 'Parse::RecDescent::UncondReject' )
                                                                                  ],
                                                                       'line' => 15
                                                                     }, 'Parse::RecDescent::Production' )
                                                            ],
                                                 'name' => 'rule',
                                                 'vars' => '',
                                                 'line' => 6
                                               }, 'Parse::RecDescent::Rule' ),
                              'program' => bless( {
                                                    'impcount' => 0,
                                                    'calls' => [
                                                                 'statement',
                                                                 'eofile'
                                                               ],
                                                    'changed' => 0,
                                                    'opcount' => 0,
                                                    'prods' => [
                                                                 bless( {
                                                                          'number' => '0',
                                                                          'strcount' => 0,
                                                                          'dircount' => 0,
                                                                          'uncommit' => undef,
                                                                          'error' => undef,
                                                                          'patcount' => 0,
                                                                          'actcount' => 1,
                                                                          'items' => [
                                                                                       bless( {
                                                                                                'subrule' => 'statement',
                                                                                                'expected' => undef,
                                                                                                'min' => 1,
                                                                                                'argcode' => undef,
                                                                                                'max' => 100000000,
                                                                                                'matchrule' => 0,
                                                                                                'repspec' => 's',
                                                                                                'lookahead' => 0,
                                                                                                'line' => 1
                                                                                              }, 'Parse::RecDescent::Repetition' ),
                                                                                       bless( {
                                                                                                'subrule' => 'eofile',
                                                                                                'matchrule' => 0,
                                                                                                'implicit' => undef,
                                                                                                'argcode' => undef,
                                                                                                'lookahead' => 0,
                                                                                                'line' => 1
                                                                                              }, 'Parse::RecDescent::Subrule' ),
                                                                                       bless( {
                                                                                                'hashname' => '__ACTION1__',
                                                                                                'lookahead' => 0,
                                                                                                'line' => 1,
                                                                                                'code' => '{ join "\\n\\n", grep $_, @{ $item[1] } }'
                                                                                              }, 'Parse::RecDescent::Action' )
                                                                                     ],
                                                                          'line' => undef
                                                                        }, 'Parse::RecDescent::Production' ),
                                                                 bless( {
                                                                          'number' => '1',
                                                                          'strcount' => 0,
                                                                          'dircount' => 1,
                                                                          'uncommit' => 0,
                                                                          'error' => 1,
                                                                          'patcount' => 0,
                                                                          'actcount' => 0,
                                                                          'items' => [
                                                                                       bless( {
                                                                                                'msg' => '',
                                                                                                'hashname' => '__DIRECTIVE1__',
                                                                                                'commitonly' => '',
                                                                                                'lookahead' => 0,
                                                                                                'line' => 2
                                                                                              }, 'Parse::RecDescent::Error' )
                                                                                     ],
                                                                          'line' => 2
                                                                        }, 'Parse::RecDescent::Production' )
                                                               ],
                                                    'name' => 'program',
                                                    'vars' => '',
                                                    'line' => 1
                                                  }, 'Parse::RecDescent::Rule' ),
                              'postfix' => bless( {
                                                    'impcount' => 0,
                                                    'calls' => [],
                                                    'changed' => 0,
                                                    'opcount' => 0,
                                                    'prods' => [
                                                                 bless( {
                                                                          'number' => '0',
                                                                          'strcount' => 1,
                                                                          'dircount' => 0,
                                                                          'uncommit' => undef,
                                                                          'error' => undef,
                                                                          'patcount' => 0,
                                                                          'actcount' => 0,
                                                                          'items' => [
                                                                                       bless( {
                                                                                                'pattern' => 'postfix',
                                                                                                'hashname' => '__STRING1__',
                                                                                                'description' => '\'postfix\'',
                                                                                                'lookahead' => 0,
                                                                                                'line' => 126
                                                                                              }, 'Parse::RecDescent::Literal' )
                                                                                     ],
                                                                          'line' => undef
                                                                        }, 'Parse::RecDescent::Production' )
                                                               ],
                                                    'name' => 'postfix',
                                                    'vars' => '',
                                                    'line' => 126
                                                  }, 'Parse::RecDescent::Rule' ),
                              'literal' => bless( {
                                                    'impcount' => 0,
                                                    'calls' => [
                                                                 'identifier',
                                                                 'number',
                                                                 'string'
                                                               ],
                                                    'changed' => 0,
                                                    'opcount' => 0,
                                                    'prods' => [
                                                                 bless( {
                                                                          'number' => '0',
                                                                          'strcount' => 0,
                                                                          'dircount' => 0,
                                                                          'uncommit' => undef,
                                                                          'error' => undef,
                                                                          'patcount' => 0,
                                                                          'actcount' => 0,
                                                                          'items' => [
                                                                                       bless( {
                                                                                                'subrule' => 'identifier',
                                                                                                'matchrule' => 0,
                                                                                                'implicit' => undef,
                                                                                                'argcode' => undef,
                                                                                                'lookahead' => 0,
                                                                                                'line' => 109
                                                                                              }, 'Parse::RecDescent::Subrule' )
                                                                                     ],
                                                                          'line' => undef
                                                                        }, 'Parse::RecDescent::Production' ),
                                                                 bless( {
                                                                          'number' => '1',
                                                                          'strcount' => 0,
                                                                          'dircount' => 0,
                                                                          'uncommit' => undef,
                                                                          'error' => undef,
                                                                          'patcount' => 0,
                                                                          'actcount' => 0,
                                                                          'items' => [
                                                                                       bless( {
                                                                                                'subrule' => 'number',
                                                                                                'matchrule' => 0,
                                                                                                'implicit' => undef,
                                                                                                'argcode' => undef,
                                                                                                'lookahead' => 0,
                                                                                                'line' => 110
                                                                                              }, 'Parse::RecDescent::Subrule' )
                                                                                     ],
                                                                          'line' => 110
                                                                        }, 'Parse::RecDescent::Production' ),
                                                                 bless( {
                                                                          'number' => '2',
                                                                          'strcount' => 0,
                                                                          'dircount' => 0,
                                                                          'uncommit' => undef,
                                                                          'error' => undef,
                                                                          'patcount' => 0,
                                                                          'actcount' => 0,
                                                                          'items' => [
                                                                                       bless( {
                                                                                                'subrule' => 'string',
                                                                                                'matchrule' => 0,
                                                                                                'implicit' => undef,
                                                                                                'argcode' => undef,
                                                                                                'lookahead' => 0,
                                                                                                'line' => 111
                                                                                              }, 'Parse::RecDescent::Subrule' )
                                                                                     ],
                                                                          'line' => 111
                                                                        }, 'Parse::RecDescent::Production' )
                                                               ],
                                                    'name' => 'literal',
                                                    'vars' => '',
                                                    'line' => 109
                                                  }, 'Parse::RecDescent::Rule' ),
                              'identifier' => bless( {
                                                       'impcount' => 0,
                                                       'calls' => [],
                                                       'changed' => 0,
                                                       'opcount' => 0,
                                                       'prods' => [
                                                                    bless( {
                                                                             'number' => '0',
                                                                             'strcount' => 0,
                                                                             'dircount' => 0,
                                                                             'uncommit' => undef,
                                                                             'error' => undef,
                                                                             'patcount' => 1,
                                                                             'actcount' => 0,
                                                                             'items' => [
                                                                                          bless( {
                                                                                                   'pattern' => '[A-Za-z_]([-\\w])*',
                                                                                                   'hashname' => '__PATTERN1__',
                                                                                                   'description' => '/[A-Za-z_]([-\\\\w])*/',
                                                                                                   'lookahead' => 0,
                                                                                                   'rdelim' => '/',
                                                                                                   'line' => 113,
                                                                                                   'mod' => '',
                                                                                                   'ldelim' => '/'
                                                                                                 }, 'Parse::RecDescent::Token' )
                                                                                        ],
                                                                             'line' => undef
                                                                           }, 'Parse::RecDescent::Production' )
                                                                  ],
                                                       'name' => 'identifier',
                                                       'vars' => '',
                                                       'line' => 113
                                                     }, 'Parse::RecDescent::Rule' ),
                              'statement' => bless( {
                                                      'impcount' => 0,
                                                      'calls' => [
                                                                   'comment',
                                                                   'directive',
                                                                   'rule',
                                                                   'facts'
                                                                 ],
                                                      'changed' => 0,
                                                      'opcount' => 0,
                                                      'prods' => [
                                                                   bless( {
                                                                            'number' => '0',
                                                                            'strcount' => 0,
                                                                            'dircount' => 0,
                                                                            'uncommit' => undef,
                                                                            'error' => undef,
                                                                            'patcount' => 0,
                                                                            'actcount' => 0,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'subrule' => 'comment',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 57
                                                                                                }, 'Parse::RecDescent::Subrule' )
                                                                                       ],
                                                                            'line' => undef
                                                                          }, 'Parse::RecDescent::Production' ),
                                                                   bless( {
                                                                            'number' => '1',
                                                                            'strcount' => 0,
                                                                            'dircount' => 0,
                                                                            'uncommit' => undef,
                                                                            'error' => undef,
                                                                            'patcount' => 0,
                                                                            'actcount' => 0,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'subrule' => 'directive',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 58
                                                                                                }, 'Parse::RecDescent::Subrule' )
                                                                                       ],
                                                                            'line' => 58
                                                                          }, 'Parse::RecDescent::Production' ),
                                                                   bless( {
                                                                            'number' => '2',
                                                                            'strcount' => 0,
                                                                            'dircount' => 0,
                                                                            'uncommit' => undef,
                                                                            'error' => undef,
                                                                            'patcount' => 0,
                                                                            'actcount' => 0,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'subrule' => 'rule',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 59
                                                                                                }, 'Parse::RecDescent::Subrule' )
                                                                                       ],
                                                                            'line' => 59
                                                                          }, 'Parse::RecDescent::Production' ),
                                                                   bless( {
                                                                            'number' => '3',
                                                                            'strcount' => 1,
                                                                            'dircount' => 0,
                                                                            'uncommit' => undef,
                                                                            'error' => undef,
                                                                            'patcount' => 1,
                                                                            'actcount' => 1,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'subrule' => 'facts',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 60
                                                                                                }, 'Parse::RecDescent::Subrule' ),
                                                                                         bless( {
                                                                                                  'pattern' => '.',
                                                                                                  'hashname' => '__STRING1__',
                                                                                                  'description' => '\'.\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 60
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'pattern' => '[\\n\\s]*',
                                                                                                  'hashname' => '__PATTERN1__',
                                                                                                  'description' => '/[\\\\n\\\\s]*/',
                                                                                                  'lookahead' => 0,
                                                                                                  'rdelim' => '/',
                                                                                                  'line' => 60,
                                                                                                  'mod' => '',
                                                                                                  'ldelim' => '/'
                                                                                                }, 'Parse::RecDescent::Token' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__ACTION1__',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 60,
                                                                                                  'code' => '{ \'\' }'
                                                                                                }, 'Parse::RecDescent::Action' )
                                                                                       ],
                                                                            'line' => 60
                                                                          }, 'Parse::RecDescent::Production' )
                                                                 ],
                                                      'name' => 'statement',
                                                      'vars' => '',
                                                      'line' => 57
                                                    }, 'Parse::RecDescent::Rule' ),
                              'predicate' => bless( {
                                                      'impcount' => 0,
                                                      'calls' => [
                                                                   'identifier',
                                                                   'arguments'
                                                                 ],
                                                      'changed' => 0,
                                                      'opcount' => 0,
                                                      'prods' => [
                                                                   bless( {
                                                                            'number' => '0',
                                                                            'strcount' => 2,
                                                                            'dircount' => 1,
                                                                            'uncommit' => undef,
                                                                            'error' => undef,
                                                                            'patcount' => 0,
                                                                            'actcount' => 1,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'subrule' => 'identifier',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 52
                                                                                                }, 'Parse::RecDescent::Subrule' ),
                                                                                         bless( {
                                                                                                  'pattern' => '(',
                                                                                                  'hashname' => '__STRING1__',
                                                                                                  'description' => '\'(\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 52
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE1__',
                                                                                                  'name' => '<commit>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 52,
                                                                                                  'code' => '$commit = 1'
                                                                                                }, 'Parse::RecDescent::Directive' ),
                                                                                         bless( {
                                                                                                  'subrule' => 'arguments',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 52
                                                                                                }, 'Parse::RecDescent::Subrule' ),
                                                                                         bless( {
                                                                                                  'pattern' => ')',
                                                                                                  'hashname' => '__STRING2__',
                                                                                                  'description' => '\')\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 52
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__ACTION1__',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 52,
                                                                                                  'code' => '{ "($item[1] $item{arguments})" }'
                                                                                                }, 'Parse::RecDescent::Action' )
                                                                                       ],
                                                                            'line' => undef
                                                                          }, 'Parse::RecDescent::Production' ),
                                                                   bless( {
                                                                            'number' => '1',
                                                                            'strcount' => 0,
                                                                            'dircount' => 2,
                                                                            'uncommit' => 0,
                                                                            'error' => 1,
                                                                            'patcount' => 0,
                                                                            'actcount' => 0,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'msg' => '',
                                                                                                  'hashname' => '__DIRECTIVE1__',
                                                                                                  'commitonly' => '?',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 53
                                                                                                }, 'Parse::RecDescent::Error' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE2__',
                                                                                                  'name' => '<reject>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 53
                                                                                                }, 'Parse::RecDescent::UncondReject' )
                                                                                       ],
                                                                            'line' => 53
                                                                          }, 'Parse::RecDescent::Production' )
                                                                 ],
                                                      'name' => 'predicate',
                                                      'vars' => '',
                                                      'line' => 52
                                                    }, 'Parse::RecDescent::Rule' ),
                              'facts' => bless( {
                                                  'impcount' => 0,
                                                  'calls' => [
                                                               'fact',
                                                               'facts'
                                                             ],
                                                  'changed' => 0,
                                                  'opcount' => 0,
                                                  'prods' => [
                                                               bless( {
                                                                        'number' => '0',
                                                                        'strcount' => 1,
                                                                        'dircount' => 1,
                                                                        'uncommit' => undef,
                                                                        'error' => undef,
                                                                        'patcount' => 0,
                                                                        'actcount' => 0,
                                                                        'items' => [
                                                                                     bless( {
                                                                                              'subrule' => 'fact',
                                                                                              'matchrule' => 0,
                                                                                              'implicit' => undef,
                                                                                              'argcode' => undef,
                                                                                              'lookahead' => 0,
                                                                                              'line' => 62
                                                                                            }, 'Parse::RecDescent::Subrule' ),
                                                                                     bless( {
                                                                                              'pattern' => ',',
                                                                                              'hashname' => '__STRING1__',
                                                                                              'description' => '\',\'',
                                                                                              'lookahead' => 0,
                                                                                              'line' => 62
                                                                                            }, 'Parse::RecDescent::Literal' ),
                                                                                     bless( {
                                                                                              'hashname' => '__DIRECTIVE1__',
                                                                                              'name' => '<commit>',
                                                                                              'lookahead' => 0,
                                                                                              'line' => 62,
                                                                                              'code' => '$commit = 1'
                                                                                            }, 'Parse::RecDescent::Directive' ),
                                                                                     bless( {
                                                                                              'subrule' => 'facts',
                                                                                              'matchrule' => 0,
                                                                                              'implicit' => undef,
                                                                                              'argcode' => undef,
                                                                                              'lookahead' => 0,
                                                                                              'line' => 62
                                                                                            }, 'Parse::RecDescent::Subrule' )
                                                                                   ],
                                                                        'line' => undef
                                                                      }, 'Parse::RecDescent::Production' ),
                                                               bless( {
                                                                        'number' => '1',
                                                                        'strcount' => 0,
                                                                        'dircount' => 0,
                                                                        'uncommit' => undef,
                                                                        'error' => undef,
                                                                        'patcount' => 0,
                                                                        'actcount' => 0,
                                                                        'items' => [
                                                                                     bless( {
                                                                                              'subrule' => 'fact',
                                                                                              'matchrule' => 0,
                                                                                              'implicit' => undef,
                                                                                              'argcode' => undef,
                                                                                              'lookahead' => 0,
                                                                                              'line' => 63
                                                                                            }, 'Parse::RecDescent::Subrule' )
                                                                                   ],
                                                                        'line' => 63
                                                                      }, 'Parse::RecDescent::Production' ),
                                                               bless( {
                                                                        'number' => '2',
                                                                        'strcount' => 0,
                                                                        'dircount' => 2,
                                                                        'uncommit' => 0,
                                                                        'error' => 1,
                                                                        'patcount' => 0,
                                                                        'actcount' => 0,
                                                                        'items' => [
                                                                                     bless( {
                                                                                              'msg' => '',
                                                                                              'hashname' => '__DIRECTIVE1__',
                                                                                              'commitonly' => '?',
                                                                                              'lookahead' => 0,
                                                                                              'line' => 64
                                                                                            }, 'Parse::RecDescent::Error' ),
                                                                                     bless( {
                                                                                              'hashname' => '__DIRECTIVE2__',
                                                                                              'name' => '<reject>',
                                                                                              'lookahead' => 0,
                                                                                              'line' => 64
                                                                                            }, 'Parse::RecDescent::UncondReject' )
                                                                                   ],
                                                                        'line' => 64
                                                                      }, 'Parse::RecDescent::Production' )
                                                             ],
                                                  'name' => 'facts',
                                                  'vars' => '',
                                                  'line' => 62
                                                }, 'Parse::RecDescent::Rule' ),
                              'atom' => bless( {
                                                 'impcount' => 0,
                                                 'calls' => [
                                                              'predicate',
                                                              'variable',
                                                              'literal'
                                                            ],
                                                 'changed' => 0,
                                                 'opcount' => 0,
                                                 'prods' => [
                                                              bless( {
                                                                       'number' => '0',
                                                                       'strcount' => 0,
                                                                       'dircount' => 0,
                                                                       'uncommit' => undef,
                                                                       'error' => undef,
                                                                       'patcount' => 0,
                                                                       'actcount' => 0,
                                                                       'items' => [
                                                                                    bless( {
                                                                                             'subrule' => 'predicate',
                                                                                             'matchrule' => 0,
                                                                                             'implicit' => undef,
                                                                                             'argcode' => undef,
                                                                                             'lookahead' => 0,
                                                                                             'line' => 48
                                                                                           }, 'Parse::RecDescent::Subrule' )
                                                                                  ],
                                                                       'line' => undef
                                                                     }, 'Parse::RecDescent::Production' ),
                                                              bless( {
                                                                       'number' => '1',
                                                                       'strcount' => 0,
                                                                       'dircount' => 0,
                                                                       'uncommit' => undef,
                                                                       'error' => undef,
                                                                       'patcount' => 0,
                                                                       'actcount' => 0,
                                                                       'items' => [
                                                                                    bless( {
                                                                                             'subrule' => 'variable',
                                                                                             'matchrule' => 0,
                                                                                             'implicit' => undef,
                                                                                             'argcode' => undef,
                                                                                             'lookahead' => 0,
                                                                                             'line' => 49
                                                                                           }, 'Parse::RecDescent::Subrule' )
                                                                                  ],
                                                                       'line' => 49
                                                                     }, 'Parse::RecDescent::Production' ),
                                                              bless( {
                                                                       'number' => '2',
                                                                       'strcount' => 0,
                                                                       'dircount' => 0,
                                                                       'uncommit' => undef,
                                                                       'error' => undef,
                                                                       'patcount' => 0,
                                                                       'actcount' => 0,
                                                                       'items' => [
                                                                                    bless( {
                                                                                             'subrule' => 'literal',
                                                                                             'matchrule' => 0,
                                                                                             'implicit' => undef,
                                                                                             'argcode' => undef,
                                                                                             'lookahead' => 0,
                                                                                             'line' => 50
                                                                                           }, 'Parse::RecDescent::Subrule' )
                                                                                  ],
                                                                       'line' => 50
                                                                     }, 'Parse::RecDescent::Production' )
                                                            ],
                                                 'name' => 'atom',
                                                 'vars' => '',
                                                 'line' => 48
                                               }, 'Parse::RecDescent::Rule' ),
                              'new_facts' => bless( {
                                                      'impcount' => 0,
                                                      'calls' => [
                                                                   'clause',
                                                                   'new_facts'
                                                                 ],
                                                      'changed' => 0,
                                                      'opcount' => 0,
                                                      'prods' => [
                                                                   bless( {
                                                                            'number' => '0',
                                                                            'strcount' => 1,
                                                                            'dircount' => 1,
                                                                            'uncommit' => undef,
                                                                            'error' => undef,
                                                                            'patcount' => 0,
                                                                            'actcount' => 1,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'subrule' => 'clause',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 91
                                                                                                }, 'Parse::RecDescent::Subrule' ),
                                                                                         bless( {
                                                                                                  'pattern' => ',',
                                                                                                  'hashname' => '__STRING1__',
                                                                                                  'description' => '\',\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 91
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE1__',
                                                                                                  'name' => '<commit>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 91,
                                                                                                  'code' => '$commit = 1'
                                                                                                }, 'Parse::RecDescent::Directive' ),
                                                                                         bless( {
                                                                                                  'subrule' => 'new_facts',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 91
                                                                                                }, 'Parse::RecDescent::Subrule' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__ACTION1__',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 93,
                                                                                                  'code' => '{ "    (assert $item{clause})\\n" . $item{new_facts} }'
                                                                                                }, 'Parse::RecDescent::Action' )
                                                                                       ],
                                                                            'line' => undef
                                                                          }, 'Parse::RecDescent::Production' ),
                                                                   bless( {
                                                                            'number' => '1',
                                                                            'strcount' => 0,
                                                                            'dircount' => 0,
                                                                            'uncommit' => undef,
                                                                            'error' => undef,
                                                                            'patcount' => 0,
                                                                            'actcount' => 1,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'subrule' => 'clause',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 95
                                                                                                }, 'Parse::RecDescent::Subrule' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__ACTION1__',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 95,
                                                                                                  'code' => '{ "    (assert $item{clause})\\n" }'
                                                                                                }, 'Parse::RecDescent::Action' )
                                                                                       ],
                                                                            'line' => 95
                                                                          }, 'Parse::RecDescent::Production' ),
                                                                   bless( {
                                                                            'number' => '2',
                                                                            'strcount' => 0,
                                                                            'dircount' => 2,
                                                                            'uncommit' => 0,
                                                                            'error' => 1,
                                                                            'patcount' => 0,
                                                                            'actcount' => 0,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'msg' => '',
                                                                                                  'hashname' => '__DIRECTIVE1__',
                                                                                                  'commitonly' => '?',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 96
                                                                                                }, 'Parse::RecDescent::Error' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE2__',
                                                                                                  'name' => '<reject>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 96
                                                                                                }, 'Parse::RecDescent::UncondReject' )
                                                                                       ],
                                                                            'line' => 96
                                                                          }, 'Parse::RecDescent::Production' )
                                                                 ],
                                                      'name' => 'new_facts',
                                                      'vars' => '',
                                                      'line' => 91
                                                    }, 'Parse::RecDescent::Rule' ),
                              'eofile' => bless( {
                                                   'impcount' => 0,
                                                   'calls' => [],
                                                   'changed' => 0,
                                                   'opcount' => 0,
                                                   'prods' => [
                                                                bless( {
                                                                         'number' => '0',
                                                                         'strcount' => 0,
                                                                         'dircount' => 0,
                                                                         'uncommit' => undef,
                                                                         'error' => undef,
                                                                         'patcount' => 1,
                                                                         'actcount' => 0,
                                                                         'items' => [
                                                                                      bless( {
                                                                                               'pattern' => '^\\Z',
                                                                                               'hashname' => '__PATTERN1__',
                                                                                               'description' => '/^\\\\Z/',
                                                                                               'lookahead' => 0,
                                                                                               'rdelim' => '/',
                                                                                               'line' => 55,
                                                                                               'mod' => '',
                                                                                               'ldelim' => '/'
                                                                                             }, 'Parse::RecDescent::Token' )
                                                                                    ],
                                                                         'line' => undef
                                                                       }, 'Parse::RecDescent::Production' )
                                                              ],
                                                   'name' => 'eofile',
                                                   'vars' => '',
                                                   'line' => 55
                                                 }, 'Parse::RecDescent::Rule' ),
                              'infix' => bless( {
                                                  'impcount' => 0,
                                                  'calls' => [
                                                               'variable'
                                                             ],
                                                  'changed' => 0,
                                                  'opcount' => 0,
                                                  'prods' => [
                                                               bless( {
                                                                        'number' => '0',
                                                                        'strcount' => 0,
                                                                        'dircount' => 0,
                                                                        'uncommit' => undef,
                                                                        'error' => undef,
                                                                        'patcount' => 0,
                                                                        'actcount' => 0,
                                                                        'items' => [
                                                                                     bless( {
                                                                                              'subrule' => 'variable',
                                                                                              'matchrule' => 0,
                                                                                              'implicit' => undef,
                                                                                              'argcode' => undef,
                                                                                              'lookahead' => 0,
                                                                                              'line' => 123
                                                                                            }, 'Parse::RecDescent::Subrule' )
                                                                                   ],
                                                                        'line' => undef
                                                                      }, 'Parse::RecDescent::Production' ),
                                                               bless( {
                                                                        'number' => '1',
                                                                        'strcount' => 0,
                                                                        'dircount' => 0,
                                                                        'uncommit' => undef,
                                                                        'error' => undef,
                                                                        'patcount' => 0,
                                                                        'actcount' => 2,
                                                                        'items' => [
                                                                                     bless( {
                                                                                              'hashname' => '__ACTION1__',
                                                                                              'lookahead' => 0,
                                                                                              'line' => 124,
                                                                                              'code' => '{ ::match_infix($text) }'
                                                                                            }, 'Parse::RecDescent::Action' ),
                                                                                     bless( {
                                                                                              'hashname' => '__ACTION2__',
                                                                                              'lookahead' => 0,
                                                                                              'line' => 124,
                                                                                              'code' => '{ $::infix{$item[1]} }'
                                                                                            }, 'Parse::RecDescent::Action' )
                                                                                   ],
                                                                        'line' => 124
                                                                      }, 'Parse::RecDescent::Production' )
                                                             ],
                                                  'name' => 'infix',
                                                  'vars' => '',
                                                  'line' => 123
                                                }, 'Parse::RecDescent::Rule' ),
                              'infix_prefix' => bless( {
                                                         'impcount' => 0,
                                                         'calls' => [],
                                                         'changed' => 0,
                                                         'opcount' => 0,
                                                         'prods' => [
                                                                      bless( {
                                                                               'number' => '0',
                                                                               'strcount' => 1,
                                                                               'dircount' => 0,
                                                                               'uncommit' => undef,
                                                                               'error' => undef,
                                                                               'patcount' => 0,
                                                                               'actcount' => 1,
                                                                               'items' => [
                                                                                            bless( {
                                                                                                     'pattern' => '@',
                                                                                                     'hashname' => '__STRING1__',
                                                                                                     'description' => '\'@\'',
                                                                                                     'lookahead' => 0,
                                                                                                     'line' => 120
                                                                                                   }, 'Parse::RecDescent::Literal' ),
                                                                                            bless( {
                                                                                                     'hashname' => '__ACTION1__',
                                                                                                     'lookahead' => 0,
                                                                                                     'line' => 120,
                                                                                                     'code' => '{ \'vector-relation\' }'
                                                                                                   }, 'Parse::RecDescent::Action' )
                                                                                          ],
                                                                               'line' => undef
                                                                             }, 'Parse::RecDescent::Production' ),
                                                                      bless( {
                                                                               'number' => '1',
                                                                               'strcount' => 1,
                                                                               'dircount' => 0,
                                                                               'uncommit' => undef,
                                                                               'error' => undef,
                                                                               'patcount' => 0,
                                                                               'actcount' => 1,
                                                                               'items' => [
                                                                                            bless( {
                                                                                                     'pattern' => '%',
                                                                                                     'hashname' => '__STRING1__',
                                                                                                     'description' => '\'%\'',
                                                                                                     'lookahead' => 0,
                                                                                                     'line' => 121
                                                                                                   }, 'Parse::RecDescent::Literal' ),
                                                                                            bless( {
                                                                                                     'hashname' => '__ACTION1__',
                                                                                                     'lookahead' => 0,
                                                                                                     'line' => 121,
                                                                                                     'code' => '{ \'space-relation\' }'
                                                                                                   }, 'Parse::RecDescent::Action' )
                                                                                          ],
                                                                               'line' => 121
                                                                             }, 'Parse::RecDescent::Production' )
                                                                    ],
                                                         'name' => 'infix_prefix',
                                                         'vars' => '',
                                                         'line' => 120
                                                       }, 'Parse::RecDescent::Rule' ),
                              'conjunction' => bless( {
                                                        'impcount' => 0,
                                                        'calls' => [
                                                                     'clause',
                                                                     'conjunction'
                                                                   ],
                                                        'changed' => 0,
                                                        'opcount' => 0,
                                                        'prods' => [
                                                                     bless( {
                                                                              'number' => '0',
                                                                              'strcount' => 1,
                                                                              'dircount' => 1,
                                                                              'uncommit' => undef,
                                                                              'error' => undef,
                                                                              'patcount' => 0,
                                                                              'actcount' => 1,
                                                                              'items' => [
                                                                                           bless( {
                                                                                                    'subrule' => 'clause',
                                                                                                    'matchrule' => 0,
                                                                                                    'implicit' => undef,
                                                                                                    'argcode' => undef,
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 24
                                                                                                  }, 'Parse::RecDescent::Subrule' ),
                                                                                           bless( {
                                                                                                    'pattern' => ',',
                                                                                                    'hashname' => '__STRING1__',
                                                                                                    'description' => '\',\'',
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 24
                                                                                                  }, 'Parse::RecDescent::Literal' ),
                                                                                           bless( {
                                                                                                    'hashname' => '__DIRECTIVE1__',
                                                                                                    'name' => '<commit>',
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 24,
                                                                                                    'code' => '$commit = 1'
                                                                                                  }, 'Parse::RecDescent::Directive' ),
                                                                                           bless( {
                                                                                                    'subrule' => 'conjunction',
                                                                                                    'matchrule' => 0,
                                                                                                    'implicit' => undef,
                                                                                                    'argcode' => undef,
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 24
                                                                                                  }, 'Parse::RecDescent::Subrule' ),
                                                                                           bless( {
                                                                                                    'hashname' => '__ACTION1__',
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 26,
                                                                                                    'code' => '{ "(and $item{clause} $item{conjunction})" }'
                                                                                                  }, 'Parse::RecDescent::Action' )
                                                                                         ],
                                                                              'line' => undef
                                                                            }, 'Parse::RecDescent::Production' ),
                                                                     bless( {
                                                                              'number' => '1',
                                                                              'strcount' => 0,
                                                                              'dircount' => 0,
                                                                              'uncommit' => undef,
                                                                              'error' => undef,
                                                                              'patcount' => 0,
                                                                              'actcount' => 0,
                                                                              'items' => [
                                                                                           bless( {
                                                                                                    'subrule' => 'clause',
                                                                                                    'matchrule' => 0,
                                                                                                    'implicit' => undef,
                                                                                                    'argcode' => undef,
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 28
                                                                                                  }, 'Parse::RecDescent::Subrule' )
                                                                                         ],
                                                                              'line' => 28
                                                                            }, 'Parse::RecDescent::Production' ),
                                                                     bless( {
                                                                              'number' => '2',
                                                                              'strcount' => 0,
                                                                              'dircount' => 2,
                                                                              'uncommit' => 0,
                                                                              'error' => 1,
                                                                              'patcount' => 0,
                                                                              'actcount' => 0,
                                                                              'items' => [
                                                                                           bless( {
                                                                                                    'msg' => '',
                                                                                                    'hashname' => '__DIRECTIVE1__',
                                                                                                    'commitonly' => '?',
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 29
                                                                                                  }, 'Parse::RecDescent::Error' ),
                                                                                           bless( {
                                                                                                    'hashname' => '__DIRECTIVE2__',
                                                                                                    'name' => '<reject>',
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 29
                                                                                                  }, 'Parse::RecDescent::UncondReject' )
                                                                                         ],
                                                                              'line' => 29
                                                                            }, 'Parse::RecDescent::Production' )
                                                                   ],
                                                        'name' => 'conjunction',
                                                        'vars' => '',
                                                        'line' => 24
                                                      }, 'Parse::RecDescent::Rule' ),
                              'compound' => bless( {
                                                     'impcount' => 0,
                                                     'calls' => [
                                                                  'clause'
                                                                ],
                                                     'changed' => 0,
                                                     'opcount' => 0,
                                                     'prods' => [
                                                                  bless( {
                                                                           'number' => '0',
                                                                           'strcount' => 0,
                                                                           'dircount' => 1,
                                                                           'uncommit' => undef,
                                                                           'error' => undef,
                                                                           'patcount' => 1,
                                                                           'actcount' => 1,
                                                                           'op' => [],
                                                                           'items' => [
                                                                                        bless( {
                                                                                                 'expected' => '<leftop: clause /([;,])/ clause>',
                                                                                                 'min' => 1,
                                                                                                 'name' => '',
                                                                                                 'max' => 100000000,
                                                                                                 'leftarg' => bless( {
                                                                                                                       'subrule' => 'clause',
                                                                                                                       'matchrule' => 0,
                                                                                                                       'implicit' => undef,
                                                                                                                       'argcode' => undef,
                                                                                                                       'lookahead' => 0,
                                                                                                                       'line' => 128
                                                                                                                     }, 'Parse::RecDescent::Subrule' ),
                                                                                                 'rightarg' => bless( {
                                                                                                                        'subrule' => 'clause',
                                                                                                                        'matchrule' => 0,
                                                                                                                        'implicit' => undef,
                                                                                                                        'argcode' => undef,
                                                                                                                        'lookahead' => 0,
                                                                                                                        'line' => 128
                                                                                                                      }, 'Parse::RecDescent::Subrule' ),
                                                                                                 'hashname' => '__DIRECTIVE1__',
                                                                                                 'type' => 'leftop',
                                                                                                 'op' => bless( {
                                                                                                                  'pattern' => '([;,])',
                                                                                                                  'hashname' => '__PATTERN1__',
                                                                                                                  'description' => '/([;,])/',
                                                                                                                  'lookahead' => 0,
                                                                                                                  'rdelim' => '/',
                                                                                                                  'line' => 128,
                                                                                                                  'mod' => '',
                                                                                                                  'ldelim' => '/'
                                                                                                                }, 'Parse::RecDescent::Token' )
                                                                                               }, 'Parse::RecDescent::Operator' ),
                                                                                        bless( {
                                                                                                 'hashname' => '__ACTION1__',
                                                                                                 'lookahead' => 0,
                                                                                                 'line' => 130,
                                                                                                 'code' => '{ "\\n    " . join "", (map { m/^[;,]$/ ? "$_\\n    " : $_ } @{ $item[1] }); }'
                                                                                               }, 'Parse::RecDescent::Action' )
                                                                                      ],
                                                                           'line' => undef
                                                                         }, 'Parse::RecDescent::Production' )
                                                                ],
                                                     'name' => 'compound',
                                                     'vars' => '',
                                                     'line' => 128
                                                   }, 'Parse::RecDescent::Rule' ),
                              'disjunction' => bless( {
                                                        'impcount' => 0,
                                                        'calls' => [
                                                                     'conjunction',
                                                                     'disjunction'
                                                                   ],
                                                        'changed' => 0,
                                                        'opcount' => 0,
                                                        'prods' => [
                                                                     bless( {
                                                                              'number' => '0',
                                                                              'strcount' => 1,
                                                                              'dircount' => 1,
                                                                              'uncommit' => undef,
                                                                              'error' => undef,
                                                                              'patcount' => 0,
                                                                              'actcount' => 1,
                                                                              'items' => [
                                                                                           bless( {
                                                                                                    'subrule' => 'conjunction',
                                                                                                    'matchrule' => 0,
                                                                                                    'implicit' => undef,
                                                                                                    'argcode' => undef,
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 17
                                                                                                  }, 'Parse::RecDescent::Subrule' ),
                                                                                           bless( {
                                                                                                    'pattern' => ';',
                                                                                                    'hashname' => '__STRING1__',
                                                                                                    'description' => '\';\'',
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 17
                                                                                                  }, 'Parse::RecDescent::Literal' ),
                                                                                           bless( {
                                                                                                    'hashname' => '__DIRECTIVE1__',
                                                                                                    'name' => '<commit>',
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 17,
                                                                                                    'code' => '$commit = 1'
                                                                                                  }, 'Parse::RecDescent::Directive' ),
                                                                                           bless( {
                                                                                                    'subrule' => 'disjunction',
                                                                                                    'matchrule' => 0,
                                                                                                    'implicit' => undef,
                                                                                                    'argcode' => undef,
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 17
                                                                                                  }, 'Parse::RecDescent::Subrule' ),
                                                                                           bless( {
                                                                                                    'hashname' => '__ACTION1__',
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 19,
                                                                                                    'code' => '{ "(or $item{conjunction} $item{disjunction})" }'
                                                                                                  }, 'Parse::RecDescent::Action' )
                                                                                         ],
                                                                              'line' => undef
                                                                            }, 'Parse::RecDescent::Production' ),
                                                                     bless( {
                                                                              'number' => '1',
                                                                              'strcount' => 0,
                                                                              'dircount' => 0,
                                                                              'uncommit' => undef,
                                                                              'error' => undef,
                                                                              'patcount' => 0,
                                                                              'actcount' => 0,
                                                                              'items' => [
                                                                                           bless( {
                                                                                                    'subrule' => 'conjunction',
                                                                                                    'matchrule' => 0,
                                                                                                    'implicit' => undef,
                                                                                                    'argcode' => undef,
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 21
                                                                                                  }, 'Parse::RecDescent::Subrule' )
                                                                                         ],
                                                                              'line' => 21
                                                                            }, 'Parse::RecDescent::Production' ),
                                                                     bless( {
                                                                              'number' => '2',
                                                                              'strcount' => 0,
                                                                              'dircount' => 2,
                                                                              'uncommit' => 0,
                                                                              'error' => 1,
                                                                              'patcount' => 0,
                                                                              'actcount' => 0,
                                                                              'items' => [
                                                                                           bless( {
                                                                                                    'msg' => '',
                                                                                                    'hashname' => '__DIRECTIVE1__',
                                                                                                    'commitonly' => '?',
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 22
                                                                                                  }, 'Parse::RecDescent::Error' ),
                                                                                           bless( {
                                                                                                    'hashname' => '__DIRECTIVE2__',
                                                                                                    'name' => '<reject>',
                                                                                                    'lookahead' => 0,
                                                                                                    'line' => 22
                                                                                                  }, 'Parse::RecDescent::UncondReject' )
                                                                                         ],
                                                                              'line' => 22
                                                                            }, 'Parse::RecDescent::Production' )
                                                                   ],
                                                        'name' => 'disjunction',
                                                        'vars' => '',
                                                        'line' => 17
                                                      }, 'Parse::RecDescent::Rule' ),
                              'general_infix' => bless( {
                                                          'impcount' => 0,
                                                          'calls' => [
                                                                       'infix_prefix',
                                                                       'infix'
                                                                     ],
                                                          'changed' => 0,
                                                          'opcount' => 0,
                                                          'prods' => [
                                                                       bless( {
                                                                                'number' => '0',
                                                                                'strcount' => 0,
                                                                                'dircount' => 1,
                                                                                'uncommit' => undef,
                                                                                'error' => undef,
                                                                                'patcount' => 0,
                                                                                'actcount' => 1,
                                                                                'items' => [
                                                                                             bless( {
                                                                                                      'subrule' => 'infix_prefix',
                                                                                                      'matchrule' => 0,
                                                                                                      'implicit' => undef,
                                                                                                      'argcode' => undef,
                                                                                                      'lookahead' => 0,
                                                                                                      'line' => 100
                                                                                                    }, 'Parse::RecDescent::Subrule' ),
                                                                                             bless( {
                                                                                                      'hashname' => '__DIRECTIVE1__',
                                                                                                      'name' => '<commit>',
                                                                                                      'lookahead' => 0,
                                                                                                      'line' => 100,
                                                                                                      'code' => '$commit = 1'
                                                                                                    }, 'Parse::RecDescent::Directive' ),
                                                                                             bless( {
                                                                                                      'subrule' => 'infix',
                                                                                                      'matchrule' => 0,
                                                                                                      'implicit' => undef,
                                                                                                      'argcode' => undef,
                                                                                                      'lookahead' => 0,
                                                                                                      'line' => 100
                                                                                                    }, 'Parse::RecDescent::Subrule' ),
                                                                                             bless( {
                                                                                                      'hashname' => '__ACTION1__',
                                                                                                      'lookahead' => 0,
                                                                                                      'line' => 100,
                                                                                                      'code' => '{ "$item{infix_prefix} $item{infix}" }'
                                                                                                    }, 'Parse::RecDescent::Action' )
                                                                                           ],
                                                                                'line' => undef
                                                                              }, 'Parse::RecDescent::Production' ),
                                                                       bless( {
                                                                                'number' => '1',
                                                                                'strcount' => 0,
                                                                                'dircount' => 0,
                                                                                'uncommit' => undef,
                                                                                'error' => undef,
                                                                                'patcount' => 0,
                                                                                'actcount' => 0,
                                                                                'items' => [
                                                                                             bless( {
                                                                                                      'subrule' => 'infix',
                                                                                                      'matchrule' => 0,
                                                                                                      'implicit' => undef,
                                                                                                      'argcode' => undef,
                                                                                                      'lookahead' => 0,
                                                                                                      'line' => 101
                                                                                                    }, 'Parse::RecDescent::Subrule' )
                                                                                           ],
                                                                                'line' => 101
                                                                              }, 'Parse::RecDescent::Production' ),
                                                                       bless( {
                                                                                'number' => '2',
                                                                                'strcount' => 0,
                                                                                'dircount' => 2,
                                                                                'uncommit' => 0,
                                                                                'error' => 1,
                                                                                'patcount' => 0,
                                                                                'actcount' => 0,
                                                                                'items' => [
                                                                                             bless( {
                                                                                                      'msg' => '',
                                                                                                      'hashname' => '__DIRECTIVE1__',
                                                                                                      'commitonly' => '?',
                                                                                                      'lookahead' => 0,
                                                                                                      'line' => 102
                                                                                                    }, 'Parse::RecDescent::Error' ),
                                                                                             bless( {
                                                                                                      'hashname' => '__DIRECTIVE2__',
                                                                                                      'name' => '<reject>',
                                                                                                      'lookahead' => 0,
                                                                                                      'line' => 102
                                                                                                    }, 'Parse::RecDescent::UncondReject' )
                                                                                           ],
                                                                                'line' => 102
                                                                              }, 'Parse::RecDescent::Production' )
                                                                     ],
                                                          'name' => 'general_infix',
                                                          'vars' => '',
                                                          'line' => 100
                                                        }, 'Parse::RecDescent::Rule' ),
                              'pattern' => bless( {
                                                    'impcount' => 0,
                                                    'calls' => [],
                                                    'changed' => 0,
                                                    'opcount' => 0,
                                                    'prods' => [
                                                                 bless( {
                                                                          'number' => '0',
                                                                          'strcount' => 0,
                                                                          'dircount' => 0,
                                                                          'uncommit' => undef,
                                                                          'error' => undef,
                                                                          'patcount' => 1,
                                                                          'actcount' => 0,
                                                                          'items' => [
                                                                                       bless( {
                                                                                                'pattern' => '\\S+(?=>)',
                                                                                                'hashname' => '__PATTERN1__',
                                                                                                'description' => '/\\\\S+(?=>)/',
                                                                                                'lookahead' => 0,
                                                                                                'rdelim' => '/',
                                                                                                'line' => 89,
                                                                                                'mod' => '',
                                                                                                'ldelim' => '/'
                                                                                              }, 'Parse::RecDescent::Token' )
                                                                                     ],
                                                                          'line' => undef
                                                                        }, 'Parse::RecDescent::Production' )
                                                               ],
                                                    'name' => 'pattern',
                                                    'vars' => '',
                                                    'line' => 89
                                                  }, 'Parse::RecDescent::Rule' ),
                              'comment' => bless( {
                                                    'impcount' => 0,
                                                    'calls' => [],
                                                    'changed' => 0,
                                                    'opcount' => 0,
                                                    'prods' => [
                                                                 bless( {
                                                                          'number' => '0',
                                                                          'strcount' => 0,
                                                                          'dircount' => 0,
                                                                          'uncommit' => undef,
                                                                          'error' => undef,
                                                                          'patcount' => 1,
                                                                          'actcount' => 1,
                                                                          'items' => [
                                                                                       bless( {
                                                                                                'pattern' => '\\/\\*(.*?)\\*\\/',
                                                                                                'hashname' => '__PATTERN1__',
                                                                                                'description' => '/\\\\/\\\\*(.*?)\\\\*\\\\//s',
                                                                                                'lookahead' => 0,
                                                                                                'rdelim' => '/',
                                                                                                'line' => 4,
                                                                                                'mod' => 's',
                                                                                                'ldelim' => '/'
                                                                                              }, 'Parse::RecDescent::Token' ),
                                                                                       bless( {
                                                                                                'hashname' => '__ACTION1__',
                                                                                                'lookahead' => 0,
                                                                                                'line' => 4,
                                                                                                'code' => '{ my $cmt = "; $1"; $cmt =~ s/\\n(?=.)/;/g; $cmt }'
                                                                                              }, 'Parse::RecDescent::Action' )
                                                                                     ],
                                                                          'line' => undef
                                                                        }, 'Parse::RecDescent::Production' )
                                                               ],
                                                    'name' => 'comment',
                                                    'vars' => '',
                                                    'line' => 4
                                                  }, 'Parse::RecDescent::Rule' ),
                              'clause' => bless( {
                                                   'impcount' => 0,
                                                   'calls' => [
                                                                'prefix',
                                                                'atom',
                                                                'general_infix',
                                                                'postfix'
                                                              ],
                                                   'changed' => 0,
                                                   'opcount' => 0,
                                                   'prods' => [
                                                                bless( {
                                                                         'number' => '0',
                                                                         'strcount' => 0,
                                                                         'dircount' => 2,
                                                                         'uncommit' => undef,
                                                                         'error' => undef,
                                                                         'patcount' => 0,
                                                                         'actcount' => 1,
                                                                         'items' => [
                                                                                      bless( {
                                                                                               'subrule' => 'prefix',
                                                                                               'matchrule' => 0,
                                                                                               'implicit' => undef,
                                                                                               'argcode' => undef,
                                                                                               'lookahead' => 0,
                                                                                               'line' => 31
                                                                                             }, 'Parse::RecDescent::Subrule' ),
                                                                                      bless( {
                                                                                               'hashname' => '__DIRECTIVE1__',
                                                                                               'name' => '<skip:\'\'>',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 31,
                                                                                               'code' => 'my $oldskip = $skip; $skip=\'\'; $oldskip'
                                                                                             }, 'Parse::RecDescent::Directive' ),
                                                                                      bless( {
                                                                                               'hashname' => '__DIRECTIVE2__',
                                                                                               'name' => '<commit>',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 31,
                                                                                               'code' => '$commit = 1'
                                                                                             }, 'Parse::RecDescent::Directive' ),
                                                                                      bless( {
                                                                                               'subrule' => 'atom',
                                                                                               'matchrule' => 0,
                                                                                               'implicit' => undef,
                                                                                               'argcode' => undef,
                                                                                               'lookahead' => 0,
                                                                                               'line' => 31
                                                                                             }, 'Parse::RecDescent::Subrule' ),
                                                                                      bless( {
                                                                                               'hashname' => '__ACTION1__',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 33,
                                                                                               'code' => '{ "($item{prefix} $item{atom})" }'
                                                                                             }, 'Parse::RecDescent::Action' )
                                                                                    ],
                                                                         'line' => undef
                                                                       }, 'Parse::RecDescent::Production' ),
                                                                bless( {
                                                                         'number' => '1',
                                                                         'strcount' => 0,
                                                                         'dircount' => 2,
                                                                         'uncommit' => undef,
                                                                         'error' => undef,
                                                                         'patcount' => 1,
                                                                         'actcount' => 1,
                                                                         'items' => [
                                                                                      bless( {
                                                                                               'subrule' => 'atom',
                                                                                               'matchrule' => 0,
                                                                                               'implicit' => undef,
                                                                                               'argcode' => undef,
                                                                                               'lookahead' => 0,
                                                                                               'line' => 35
                                                                                             }, 'Parse::RecDescent::Subrule' ),
                                                                                      bless( {
                                                                                               'hashname' => '__DIRECTIVE1__',
                                                                                               'name' => '<skip:\'\'>',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 35,
                                                                                               'code' => 'my $oldskip = $skip; $skip=\'\'; $oldskip'
                                                                                             }, 'Parse::RecDescent::Directive' ),
                                                                                      bless( {
                                                                                               'pattern' => '\\s+',
                                                                                               'hashname' => '__PATTERN1__',
                                                                                               'description' => '/\\\\s+/',
                                                                                               'lookahead' => 0,
                                                                                               'rdelim' => '/',
                                                                                               'line' => 35,
                                                                                               'mod' => '',
                                                                                               'ldelim' => '/'
                                                                                             }, 'Parse::RecDescent::Token' ),
                                                                                      bless( {
                                                                                               'subrule' => 'general_infix',
                                                                                               'matchrule' => 0,
                                                                                               'implicit' => undef,
                                                                                               'argcode' => undef,
                                                                                               'lookahead' => 0,
                                                                                               'line' => 35
                                                                                             }, 'Parse::RecDescent::Subrule' ),
                                                                                      bless( {
                                                                                               'hashname' => '__DIRECTIVE2__',
                                                                                               'name' => '<skip:\'\\s*\'>',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 35,
                                                                                               'code' => 'my $oldskip = $skip; $skip=\'\\s*\'; $oldskip'
                                                                                             }, 'Parse::RecDescent::Directive' ),
                                                                                      bless( {
                                                                                               'subrule' => 'atom',
                                                                                               'matchrule' => 0,
                                                                                               'implicit' => undef,
                                                                                               'argcode' => undef,
                                                                                               'lookahead' => 0,
                                                                                               'line' => 35
                                                                                             }, 'Parse::RecDescent::Subrule' ),
                                                                                      bless( {
                                                                                               'hashname' => '__ACTION1__',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 37,
                                                                                               'code' => '{ "($item{general_infix} $item[1] $item[6])" }'
                                                                                             }, 'Parse::RecDescent::Action' )
                                                                                    ],
                                                                         'line' => 35
                                                                       }, 'Parse::RecDescent::Production' ),
                                                                bless( {
                                                                         'number' => '2',
                                                                         'strcount' => 0,
                                                                         'dircount' => 1,
                                                                         'uncommit' => undef,
                                                                         'error' => undef,
                                                                         'patcount' => 0,
                                                                         'actcount' => 1,
                                                                         'items' => [
                                                                                      bless( {
                                                                                               'subrule' => 'atom',
                                                                                               'matchrule' => 0,
                                                                                               'implicit' => undef,
                                                                                               'argcode' => undef,
                                                                                               'lookahead' => 0,
                                                                                               'line' => 39
                                                                                             }, 'Parse::RecDescent::Subrule' ),
                                                                                      bless( {
                                                                                               'hashname' => '__DIRECTIVE1__',
                                                                                               'name' => '<skip:\'\'>',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 39,
                                                                                               'code' => 'my $oldskip = $skip; $skip=\'\'; $oldskip'
                                                                                             }, 'Parse::RecDescent::Directive' ),
                                                                                      bless( {
                                                                                               'subrule' => 'postfix',
                                                                                               'matchrule' => 0,
                                                                                               'implicit' => undef,
                                                                                               'argcode' => undef,
                                                                                               'lookahead' => 0,
                                                                                               'line' => 39
                                                                                             }, 'Parse::RecDescent::Subrule' ),
                                                                                      bless( {
                                                                                               'hashname' => '__ACTION1__',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 41,
                                                                                               'code' => '{ "($item{postfix} $item{atom})" }'
                                                                                             }, 'Parse::RecDescent::Action' )
                                                                                    ],
                                                                         'line' => 39
                                                                       }, 'Parse::RecDescent::Production' ),
                                                                bless( {
                                                                         'number' => '3',
                                                                         'strcount' => 0,
                                                                         'dircount' => 0,
                                                                         'uncommit' => undef,
                                                                         'error' => undef,
                                                                         'patcount' => 0,
                                                                         'actcount' => 0,
                                                                         'items' => [
                                                                                      bless( {
                                                                                               'subrule' => 'atom',
                                                                                               'matchrule' => 0,
                                                                                               'implicit' => undef,
                                                                                               'argcode' => undef,
                                                                                               'lookahead' => 0,
                                                                                               'line' => 43
                                                                                             }, 'Parse::RecDescent::Subrule' )
                                                                                    ],
                                                                         'line' => 43
                                                                       }, 'Parse::RecDescent::Production' ),
                                                                bless( {
                                                                         'number' => '4',
                                                                         'strcount' => 0,
                                                                         'dircount' => 2,
                                                                         'uncommit' => 0,
                                                                         'error' => 1,
                                                                         'patcount' => 0,
                                                                         'actcount' => 0,
                                                                         'items' => [
                                                                                      bless( {
                                                                                               'msg' => '',
                                                                                               'hashname' => '__DIRECTIVE1__',
                                                                                               'commitonly' => '?',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 44
                                                                                             }, 'Parse::RecDescent::Error' ),
                                                                                      bless( {
                                                                                               'hashname' => '__DIRECTIVE2__',
                                                                                               'name' => '<reject>',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 44
                                                                                             }, 'Parse::RecDescent::UncondReject' )
                                                                                    ],
                                                                         'line' => 44
                                                                       }, 'Parse::RecDescent::Production' )
                                                              ],
                                                   'name' => 'clause',
                                                   'vars' => '',
                                                   'line' => 31
                                                 }, 'Parse::RecDescent::Rule' ),
                              'arguments' => bless( {
                                                      'impcount' => 0,
                                                      'calls' => [
                                                                   'clause'
                                                                 ],
                                                      'changed' => 0,
                                                      'opcount' => 0,
                                                      'prods' => [
                                                                   bless( {
                                                                            'number' => '0',
                                                                            'strcount' => 1,
                                                                            'dircount' => 1,
                                                                            'uncommit' => undef,
                                                                            'error' => undef,
                                                                            'patcount' => 0,
                                                                            'actcount' => 1,
                                                                            'op' => [],
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'expected' => '<leftop: clause \',\' clause>',
                                                                                                  'min' => 1,
                                                                                                  'name' => '',
                                                                                                  'max' => 100000000,
                                                                                                  'leftarg' => bless( {
                                                                                                                        'subrule' => 'clause',
                                                                                                                        'matchrule' => 0,
                                                                                                                        'implicit' => undef,
                                                                                                                        'argcode' => undef,
                                                                                                                        'lookahead' => 0,
                                                                                                                        'line' => 104
                                                                                                                      }, 'Parse::RecDescent::Subrule' ),
                                                                                                  'rightarg' => bless( {
                                                                                                                         'subrule' => 'clause',
                                                                                                                         'matchrule' => 0,
                                                                                                                         'implicit' => undef,
                                                                                                                         'argcode' => undef,
                                                                                                                         'lookahead' => 0,
                                                                                                                         'line' => 104
                                                                                                                       }, 'Parse::RecDescent::Subrule' ),
                                                                                                  'hashname' => '__DIRECTIVE1__',
                                                                                                  'type' => 'leftop',
                                                                                                  'op' => bless( {
                                                                                                                   'pattern' => ',',
                                                                                                                   'hashname' => '__STRING1__',
                                                                                                                   'description' => '\',\'',
                                                                                                                   'lookahead' => 0,
                                                                                                                   'line' => 104
                                                                                                                 }, 'Parse::RecDescent::Literal' )
                                                                                                }, 'Parse::RecDescent::Operator' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__ACTION1__',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 104,
                                                                                                  'code' => '{ join \' \', @{ $item[1] } }'
                                                                                                }, 'Parse::RecDescent::Action' )
                                                                                       ],
                                                                            'line' => undef
                                                                          }, 'Parse::RecDescent::Production' )
                                                                 ],
                                                      'name' => 'arguments',
                                                      'vars' => '',
                                                      'line' => 104
                                                    }, 'Parse::RecDescent::Rule' ),
                              'directive' => bless( {
                                                      'impcount' => 0,
                                                      'calls' => [
                                                                   'identifier',
                                                                   'string',
                                                                   'pattern'
                                                                 ],
                                                      'changed' => 0,
                                                      'opcount' => 0,
                                                      'prods' => [
                                                                   bless( {
                                                                            'number' => '0',
                                                                            'strcount' => 2,
                                                                            'dircount' => 1,
                                                                            'uncommit' => undef,
                                                                            'error' => undef,
                                                                            'patcount' => 1,
                                                                            'actcount' => 1,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'pattern' => 'module',
                                                                                                  'hashname' => '__STRING1__',
                                                                                                  'description' => '\'module\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 71
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE1__',
                                                                                                  'name' => '<commit>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 71,
                                                                                                  'code' => '$commit = 1'
                                                                                                }, 'Parse::RecDescent::Directive' ),
                                                                                         bless( {
                                                                                                  'subrule' => 'identifier',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 71
                                                                                                }, 'Parse::RecDescent::Subrule' ),
                                                                                         bless( {
                                                                                                  'pattern' => '.',
                                                                                                  'hashname' => '__STRING2__',
                                                                                                  'description' => '\'.\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 71
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'pattern' => '[\\n\\s]*',
                                                                                                  'hashname' => '__PATTERN1__',
                                                                                                  'description' => '/[\\\\n\\\\s]*/',
                                                                                                  'lookahead' => 0,
                                                                                                  'rdelim' => '/',
                                                                                                  'line' => 71,
                                                                                                  'mod' => '',
                                                                                                  'ldelim' => '/'
                                                                                                }, 'Parse::RecDescent::Token' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__ACTION1__',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 73,
                                                                                                  'code' => '{ $::module = uc($item{identifier}) . \'::\'; \'\' }'
                                                                                                }, 'Parse::RecDescent::Action' )
                                                                                       ],
                                                                            'line' => undef
                                                                          }, 'Parse::RecDescent::Production' ),
                                                                   bless( {
                                                                            'number' => '1',
                                                                            'strcount' => 2,
                                                                            'dircount' => 1,
                                                                            'uncommit' => undef,
                                                                            'error' => undef,
                                                                            'patcount' => 1,
                                                                            'actcount' => 1,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'pattern' => 'include',
                                                                                                  'hashname' => '__STRING1__',
                                                                                                  'description' => '\'include\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 75
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE1__',
                                                                                                  'name' => '<commit>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 75,
                                                                                                  'code' => '$commit = 1'
                                                                                                }, 'Parse::RecDescent::Directive' ),
                                                                                         bless( {
                                                                                                  'subrule' => 'string',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 75
                                                                                                }, 'Parse::RecDescent::Subrule' ),
                                                                                         bless( {
                                                                                                  'pattern' => '.',
                                                                                                  'hashname' => '__STRING2__',
                                                                                                  'description' => '\'.\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 75
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'pattern' => '[\\n\\s]*',
                                                                                                  'hashname' => '__PATTERN1__',
                                                                                                  'description' => '/[\\\\n\\\\s]*/',
                                                                                                  'lookahead' => 0,
                                                                                                  'rdelim' => '/',
                                                                                                  'line' => 75,
                                                                                                  'mod' => '',
                                                                                                  'ldelim' => '/'
                                                                                                }, 'Parse::RecDescent::Token' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__ACTION1__',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 77,
                                                                                                  'code' => '{ my $res = ::process_include(eval $item{string}, $itempos[1]{line}{from}) }'
                                                                                                }, 'Parse::RecDescent::Action' )
                                                                                       ],
                                                                            'line' => 75
                                                                          }, 'Parse::RecDescent::Production' ),
                                                                   bless( {
                                                                            'number' => '2',
                                                                            'strcount' => 3,
                                                                            'dircount' => 3,
                                                                            'uncommit' => undef,
                                                                            'error' => undef,
                                                                            'patcount' => 1,
                                                                            'actcount' => 1,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'pattern' => 'prefix:<',
                                                                                                  'hashname' => '__STRING1__',
                                                                                                  'description' => '\'prefix:<\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 79
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE1__',
                                                                                                  'name' => '<commit>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 79,
                                                                                                  'code' => '$commit = 1'
                                                                                                }, 'Parse::RecDescent::Directive' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE2__',
                                                                                                  'name' => '<skip:\'\'>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 79,
                                                                                                  'code' => 'my $oldskip = $skip; $skip=\'\'; $oldskip'
                                                                                                }, 'Parse::RecDescent::Directive' ),
                                                                                         bless( {
                                                                                                  'subrule' => 'pattern',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 79
                                                                                                }, 'Parse::RecDescent::Subrule' ),
                                                                                         bless( {
                                                                                                  'pattern' => '>',
                                                                                                  'hashname' => '__STRING2__',
                                                                                                  'description' => '\'>\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 79
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE3__',
                                                                                                  'name' => '<skip:\'\\s*\'>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 79,
                                                                                                  'code' => 'my $oldskip = $skip; $skip=\'\\s*\'; $oldskip'
                                                                                                }, 'Parse::RecDescent::Directive' ),
                                                                                         bless( {
                                                                                                  'subrule' => 'string',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 79
                                                                                                }, 'Parse::RecDescent::Subrule' ),
                                                                                         bless( {
                                                                                                  'pattern' => '.',
                                                                                                  'hashname' => '__STRING3__',
                                                                                                  'description' => '\'.\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 79
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'pattern' => '[\\n\\s]*',
                                                                                                  'hashname' => '__PATTERN1__',
                                                                                                  'description' => '/[\\\\n\\\\s]*/',
                                                                                                  'lookahead' => 0,
                                                                                                  'rdelim' => '/',
                                                                                                  'line' => 79,
                                                                                                  'mod' => '',
                                                                                                  'ldelim' => '/'
                                                                                                }, 'Parse::RecDescent::Token' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__ACTION1__',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 81,
                                                                                                  'code' => '{ $::prefix{$item{pattern}} = eval $item{string}; \'\' }'
                                                                                                }, 'Parse::RecDescent::Action' )
                                                                                       ],
                                                                            'line' => 79
                                                                          }, 'Parse::RecDescent::Production' ),
                                                                   bless( {
                                                                            'number' => '3',
                                                                            'strcount' => 3,
                                                                            'dircount' => 3,
                                                                            'uncommit' => undef,
                                                                            'error' => undef,
                                                                            'patcount' => 1,
                                                                            'actcount' => 1,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'pattern' => 'infix:<',
                                                                                                  'hashname' => '__STRING1__',
                                                                                                  'description' => '\'infix:<\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 83
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE1__',
                                                                                                  'name' => '<commit>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 83,
                                                                                                  'code' => '$commit = 1'
                                                                                                }, 'Parse::RecDescent::Directive' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE2__',
                                                                                                  'name' => '<skip:\'\'>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 83,
                                                                                                  'code' => 'my $oldskip = $skip; $skip=\'\'; $oldskip'
                                                                                                }, 'Parse::RecDescent::Directive' ),
                                                                                         bless( {
                                                                                                  'subrule' => 'pattern',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 83
                                                                                                }, 'Parse::RecDescent::Subrule' ),
                                                                                         bless( {
                                                                                                  'pattern' => '>',
                                                                                                  'hashname' => '__STRING2__',
                                                                                                  'description' => '\'>\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 83
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE3__',
                                                                                                  'name' => '<skip:\'\\s*\'>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 83,
                                                                                                  'code' => 'my $oldskip = $skip; $skip=\'\\s*\'; $oldskip'
                                                                                                }, 'Parse::RecDescent::Directive' ),
                                                                                         bless( {
                                                                                                  'subrule' => 'string',
                                                                                                  'matchrule' => 0,
                                                                                                  'implicit' => undef,
                                                                                                  'argcode' => undef,
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 83
                                                                                                }, 'Parse::RecDescent::Subrule' ),
                                                                                         bless( {
                                                                                                  'pattern' => '.',
                                                                                                  'hashname' => '__STRING3__',
                                                                                                  'description' => '\'.\'',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 83
                                                                                                }, 'Parse::RecDescent::Literal' ),
                                                                                         bless( {
                                                                                                  'pattern' => '[\\n\\s]*',
                                                                                                  'hashname' => '__PATTERN1__',
                                                                                                  'description' => '/[\\\\n\\\\s]*/',
                                                                                                  'lookahead' => 0,
                                                                                                  'rdelim' => '/',
                                                                                                  'line' => 83,
                                                                                                  'mod' => '',
                                                                                                  'ldelim' => '/'
                                                                                                }, 'Parse::RecDescent::Token' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__ACTION1__',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 85,
                                                                                                  'code' => '{ $::infix{$item{pattern}} = eval $item{string}; \'\' }'
                                                                                                }, 'Parse::RecDescent::Action' )
                                                                                       ],
                                                                            'line' => 83
                                                                          }, 'Parse::RecDescent::Production' ),
                                                                   bless( {
                                                                            'number' => '4',
                                                                            'strcount' => 0,
                                                                            'dircount' => 2,
                                                                            'uncommit' => 0,
                                                                            'error' => 1,
                                                                            'patcount' => 0,
                                                                            'actcount' => 0,
                                                                            'items' => [
                                                                                         bless( {
                                                                                                  'msg' => '',
                                                                                                  'hashname' => '__DIRECTIVE1__',
                                                                                                  'commitonly' => '?',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 87
                                                                                                }, 'Parse::RecDescent::Error' ),
                                                                                         bless( {
                                                                                                  'hashname' => '__DIRECTIVE2__',
                                                                                                  'name' => '<reject>',
                                                                                                  'lookahead' => 0,
                                                                                                  'line' => 87
                                                                                                }, 'Parse::RecDescent::UncondReject' )
                                                                                       ],
                                                                            'line' => 87
                                                                          }, 'Parse::RecDescent::Production' )
                                                                 ],
                                                      'name' => 'directive',
                                                      'vars' => '',
                                                      'line' => 71
                                                    }, 'Parse::RecDescent::Rule' ),
                              'prefix' => bless( {
                                                   'impcount' => 0,
                                                   'calls' => [],
                                                   'changed' => 0,
                                                   'opcount' => 0,
                                                   'prods' => [
                                                                bless( {
                                                                         'number' => '0',
                                                                         'strcount' => 0,
                                                                         'dircount' => 0,
                                                                         'uncommit' => undef,
                                                                         'error' => undef,
                                                                         'patcount' => 0,
                                                                         'actcount' => 2,
                                                                         'items' => [
                                                                                      bless( {
                                                                                               'hashname' => '__ACTION1__',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 46,
                                                                                               'code' => '{ ::match_prefix($text) }'
                                                                                             }, 'Parse::RecDescent::Action' ),
                                                                                      bless( {
                                                                                               'hashname' => '__ACTION2__',
                                                                                               'lookahead' => 0,
                                                                                               'line' => 46,
                                                                                               'code' => '{ $::prefix{$item[1]} }'
                                                                                             }, 'Parse::RecDescent::Action' )
                                                                                    ],
                                                                         'line' => undef
                                                                       }, 'Parse::RecDescent::Production' )
                                                              ],
                                                   'name' => 'prefix',
                                                   'vars' => '',
                                                   'line' => 46
                                                 }, 'Parse::RecDescent::Rule' )
                            }
               }, 'Parse::RecDescent' );
}
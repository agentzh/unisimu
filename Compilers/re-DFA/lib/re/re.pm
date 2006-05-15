#: re/re.pm
#: Regexp emitter for re
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-13 2006-05-15

package re::re;

use strict;
use warnings;
#use Data::Dumper::Simple;

use re;
use Language::AttributeGrammar;
#use Scalar::Util qw( looks_like_number );

sub translate {
    my $src = shift;
    #warn $src;
    my $parser = re::Parser->new() or die "Can't construct the parser!\n";
    my $ptree = $parser->program($src) or return undef;
    return emit($ptree);
}

sub emit {
    my $ptree = shift;
    $Data::Dumper::Indent = 1;
    #warn Dumper($ptree);
    $re::re::emit::grammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

program:    $/.re = { $<expression>.re }

expression: $/.re = { $<alternation>.re }
alternation:     $/.re = { re::re::emit_alternation( $<alternation>.re, $<concat>.re ) }
concat:     $/.re = { $<concat>.re . $<modified_atom>.re }

modified_atom:  $/.re = { $<atom>.re . $<modifier>.re; }
atom:           $/.re = { re::re::emit_atom( $<child>.re ) }
modifier:       $/.re = { $<__VALUE__> }

char:           $/.re = { $<__VALUE__> }

nil:        $/.re = { '' }

END_GRAMMAR
    $re::re::emit::grammar->apply($ptree, 're');
}

sub emit_alternation {
    my ($a, $b) = @_;
    if ($a) {
        "$a|$b";
    } else {
        $b;
    }
}

sub emit_atom {
    my ($re) = @_;
    if ($re =~ /^(?:\\.|[^*|()])$/ or $re =~ /^\(.*\)$/) {
        $re;
    } else {
        "($re)";
    }
}

1;
__END__

#: xml/XML.pm
#: Regexp emitter for xml
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-13 2006-05-17

package re::XML;

use strict;
use warnings;
#use Data::Dumper::Simple;

use re::Parser;
use re::AST;
use Language::AttributeGrammar;
#use Scalar::Util qw( looks_like_number );

sub translate {
    my ($self, $src) = @_;
    #warn $src;
    my $parser = re::Parser->new() or die "Can't construct the parser!\n";
    my $ptree = $parser->program($src) or return undef;
    return $self->emit($ptree);
}

sub emit {
    my ($self, $ptree) = @_;
    #warn Dumper($ptree);
    $xml::xml::emit::grammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

program:    $/.xml = { "<expression>\n" . $<expression>.xml . "</expression>\n" }

expression: $/.xml = { "<alternation>\n" . $<alternation>.xml . "</alternation>\n"; }
alternation:     $/.xml = { $<alternation>.xml . "<concat>\n" . $<concat>.xml . "</concat>\n"; }
concat:     $/.xml = { $<concat>.xml . $<modified_atom>.xml; }

modified_atom:  $/.xml = { "<modified_atom>\n" . $<atom>.xml . $<modifier>.xml . "</modified_atom>\n"; }
atom:           $/.xml = { re::XML::emit_atom( $/, $<child>.xml ); }
modifier:       $/.xml = { "<modifier>" . $<__VALUE__> . "</modifier>\n"; }

char:           $/.xml = { "<char>" . re::XML::escape( $<__VALUE__> ) . "</char>\n"; }

nil:        $/.xml = { '' }

END_GRAMMAR
    $xml::xml::emit::grammar->apply($ptree, 'xml');
}

sub emit_atom {
    my ($self, $re) = @_;
    my $xml = '';
    #warn "YEAH YEAH YEAH $self";
    if (defined $self->{expression}) {
        #warn "HERE!!!!";
        $xml .= "<expression>\n$re</expression>\n";
    } else {
        $xml .= $re;
    }
    "<atom>\n$xml</atom>\n";
}

sub escape {
    my $s = shift;
    #warn "++ ESCAPE: $s";
    $s =~ s/\&/\&amp;/g;
    $s =~ s/</\&lt;/g;
    $s =~ s/>/\&gt;/g;
    $s =~ s/"/&quot;/g;
    $s =~ s/'/&apos;/g;
    #$s =~ s/ /&nbsp;/g;
    #warn "-- ESCAPE: $s";
    return $s;
}

1;
__END__

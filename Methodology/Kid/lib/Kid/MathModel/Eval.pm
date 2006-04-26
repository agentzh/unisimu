#: Kid/MathModel/Eval.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-26 2006-04-26

package Kid::MathModel::Eval;

use strict;
use warnings;
use Kid::MathModel;
use PerlMaple;

our @Logs;

sub log_code {
    push @Logs, ">> @_";
    "@_";
}

sub log_ans {
    push @Logs, "\t@_";
    wantarray ? @_ : $_[0];
}

sub log_comment {
    push @Logs, "# @_";
    "@_";
}

sub eval_mm {
    my $mm_ast = shift;

    my $maple = PerlMaple->new;
    $maple->eval_cmd('infolevel[solve]:=1;');

    my @mms;
    for my $set (@$mm_ast) {
        my @inits   = sort @{ $set->{init_vars}   };
        my @finals  = sort @{ $set->{final_vars}  };
        
        next if ! @finals;

        my @rels    = Kid::MathModel::to_maple( @{ $set->{relationals}} );
        my @assigns = Kid::MathModel::to_maple( @{ $set->{assignments}} );

        my $mplcode = log_code join(';', @assigns).';';
        log_ans $maple->eval_cmd($mplcode);

        my $rels = '{' . join(',', @rels) . '}';
        $mplcode = log_code "solve($rels, {" . join(',', @inits) . '});';
        $maple->ReturnAST(1);
        my $set = log_ans $maple->eval_cmd($mplcode);

        my (@lhs, @rhs);
        for my $final (@finals) {
            my $value = log_ans $maple->eval_cmd( log_code "$final;" );
            $value = denumber($maple, $value, @inits);
            push @rhs, $value;
            (my $var = $final) =~ s/_\d+$//;
            push @lhs, $var;
        }

        my @sols;
        if ($set and $set->type('set')) {
            $set = denumber($maple, $set, @inits);
            foreach ($set->ops) {
                if ($maple->evalb($_) ne 'true') {
                    push @sols, "$_";
                }
            }
        } elsif ($set =~ /solutions may have been lost/) {
            push @sols, $rels;
        } elsif ($set =~ /no solutions found/) {
            warn log_comment "eval_mm: illegalize $rels";
        } else {
            warn log_comment "eval_mm: unexpected result: $set";
        }
        @sols = sort @sols;
        push @mms, { conditions => \@sols, lhs => \@lhs, rhs => \@rhs };
    }
    \@mms;
}

sub denumber {
    my $maple = shift;
    my $expr  = shift;
    my @inits = @_;
    my @raws = map { /(.+)_\d+$/; "$_=$1" } @inits;
    my $mplcode = log_code "eval($expr, {" . join(',', @raws) . '});';
    log_ans $maple->eval_cmd($mplcode);
}

sub translate {
    my $src = $_[0];
    #warn $src;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $parse_tree = $parser->program( $src ) or return undef;
    my $logic_ast = Kid::Logic::transform( $parse_tree );
    my $disjoint_ast = Kid::Logic::Disjoint::transform( $logic_ast );
    my $mm_ast  = Kid::MathModel::transform( $disjoint_ast );
    my $mms_ast = eval_mm( $mm_ast );
    my $s = '';
    for (@$mms_ast) {
        my $conds   = join(', ', @{ $_->{conditions} });
        my $lhs = join(', ', @{ $_->{lhs} });
        my $rhs = join(', ', @{ $_->{rhs} });
        $s .= <<_EOC_;
--
 - $conds
 - $lhs := $rhs
_EOC_
    }
    open my $out, ">> eval.log" or
        die "Can't open tmp.log for writing: $!";
    print $out join("\n", @Logs);
    close $out;
    $s =~ s/\s+\n/\n/sg;
    $s;
}

1;
__END__

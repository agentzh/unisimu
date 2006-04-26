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
    #log_comment(caller);
    "@_";
}

sub log_ans {
    push @Logs, "\t@_";
    #log_comment(caller);
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
        my @finals  = sort @{ $set->{final_vars}  };
        # if there's no final var at all, it's a no-op
        next if ! @finals;

        my @assigns = Kid::MathModel::to_maple( @{ $set->{assignments}} );
        # if there's no assignment at all, it's also a no-op
        next if ! @assigns;

        my @rels    = Kid::MathModel::to_maple( @{ $set->{relationals}} );
        my @inits   = sort @{ $set->{init_vars}   };

        my $mplcode = log_code join(';', @assigns).';';
        log_ans $maple->eval_cmd($mplcode);

        my (@lhs, @rhs);
        for my $final (@finals) {
            next if !$final or $final =~ /_0$/;
            my $value = log_ans $maple->eval_cmd( log_code "$final;" );
            if ($value =~ /[A-Za-z_]/) {
                $value = denumber($maple, $value, @inits);
            }
            push @rhs, $value;
            (my $var = $final) =~ s/_\d+$//;
            push @lhs, $var;
        }

        my @sols;
        $maple->ReturnAST(1);
        if (@rels) {
            # Maple's solve function may filter out inequalities in the form
            # x <> y, so we have to preserve them manually
            my @neqs;
            for (@rels) {
                next if !/<>/;
                push @neqs, denumber($maple, $_, @inits);
            }
            my $rels = '{' . join(',', @rels) . '}';
            $mplcode = log_code "solve($rels, {" . join(',', @inits) . '});';
            my $set = log_ans $maple->eval_cmd($mplcode);

            if ($set and $set->type('set')) {
                $set = denumber($maple, $set, @inits);
                foreach ($set->ops) {
                    if ($maple->evalb($_) ne 'true') {
                        my $sol = "$_";
                        $sol =~ s/\s+//g;
                        push @sols, $sol;
                    }
                }
            } elsif ($set =~ /solutions may have been lost/) {
                push @sols, $rels;
            } elsif ($set =~ /no solutions found/) {
                log_comment("Inconsistent model: $rels");
                next;
            } else {
                warn log_comment("eval_mm: unexpected result: $set"), "\n";
                next;
            }
            push @sols, map { s/\s+//g; $_ } @neqs;
            @sols = sort @sols;
        }
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
        my $conds = join(', ', @{ $_->{conditions} });
        my $lhs = join(', ', @{ $_->{lhs} });
        my $rhs = join(', ', @{ $_->{rhs} });
        my $assign = "$lhs := $rhs";
        $s .= <<_EOC_;
--
 - $conds
 - $assign
_EOC_
    }
    open my $out, ">> maple.log" or
        die "Can't open tmp.log for writing: $!";
    print $out "----" x 5, " ", scalar(localtime),
        " ", "----" x 5, "\n\n";
    print $out join("\n", @Logs), "\n\n";
    close $out;
    @Logs = ();
    $s =~ s/\s+\n/\n/sg;
    $s;
}

1;
__END__

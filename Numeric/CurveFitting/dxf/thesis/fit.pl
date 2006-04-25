use strict;
use warnings;

use Data::Dumper;
use PerlMaple;

my @lines = <STDIN>;
map { chomp; $_ = [split(/\s+/, $_)]; } @lines;
$Data::Dumper::Indent = 0;
my $s = Dumper(\@lines);
$s =~ s/\$VAR1 =/points :=/;
$s =~ s/'//g;
$s =~ s/\],/\],\n/g;
$s =~ s/\[\],\n//g;
warn $s;
#die;

my $maple = PerlMaple->new;

$maple->eval_cmd(<<_EOC_);
    calc_err := proc()
        evalf(convert(map(i->( i[2] - eval(f(i[1]), I=0) )^2, points), `+`));
    end proc:
_EOC_

print "\n\t", join("\n\t", fit_curve_between($s, 2, 3, 0.005));
plot_curve('plot.gif');

sub fit_curve {
    my ($points, $degree) = @_;
    $maple->eval_cmd(<<".");
$points;
with(CurveFitting):
LeastSquares(points, x, curve=a*ln(x+1)^$degree):
c := evalf(%);
c := eval(c, I=0);
f := unapply(c, x);
.
    return $maple->eval_cmd("c;");
}

sub fit_curve_between {
    my ($points, $left, $right, $step) = @_;
    $maple->ReturnAST(0);
    my @errs;
    $b = $left;
    while ($b <= $right) {
        my @rec = fit_curve($points, $b);
        push @rec, $b;
        push @rec, $maple->calc_err();
        push @errs, [@rec];
        $b += $step;
    }
    @errs = sort { $a->[2] <=> $b->[2] } @errs;
    return (@{ $errs[0] });
}

sub plot_curve {
    my $imfile = shift;
    $maple->eval_cmd(<<".");
with(plots):
plotsetup(gif, plotoutput="$imfile", plotoptions="height=300, width=600"):
pplot := pointplot(points, symbol=CIRCLE, color=black):
cplot := plot(c, x=0..1200, color=red):
display([cplot, pplot], axes=BOXED, scaling=CONSTRAINED);
.
}

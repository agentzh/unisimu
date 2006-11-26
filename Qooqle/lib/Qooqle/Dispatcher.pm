package Qooqle::Dispatcher;
use Jifty::Dispatcher -base;

my $ME = __PACKAGE__;

# Default page
on '/**', run {
    if ($ME->template_exists($1) or $1 =~ /^__jifty/) {
        show;
    } else {
        redirect('/search') ;
    }
};

1;

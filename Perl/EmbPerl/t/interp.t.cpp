//: interp.t.cpp
//: Test the Perl::Interp class in the embperl library
//: Copyright (c) 2004-2006 Agent Zhang
//: 2004-03-23 2006-04-10

#include <embperl.h>
#include <string>
#include <cpptest.h>

int main( int argc, char* argv[], char* env[] ){

    test_plan(13);

    {
        // test the Perl::Interp class:
        Perl::Interp pl;

        is( (int) pl.eval(" 1+2; "), 3 )

        std::string s = pl.eval(" 'a' x 3; ");
        is( s.c_str(), "aaa" );
        is( (double) pl.eval(" 3.0/2.0; "), 1.500 );
        Perl::SV sv = pl.SV("a");
        sv = pl.eval( "4*3;" );
        is( (int)pl.SV("a"), 12 );
        is( (int)sv, 12 );
        is( sv.c_str(), "12");

        pl.SV("a") = "hello!";
        is( (int)sv, 0 );
        is( sv.c_str(), "hello!" );

        pl.eval( "@b = (4,5,6);" );
        Perl::AV av = pl.AV("b");
        pl.eval( "push @b, 7;" );
        is( av.length(), 4 );
        is( (int)av[1], 5 );
        is( av[3].c_str(), "7" );

        pl.eval( "%c = ('Tom'=>'mail','Mary'=>'femail');" );
        Perl::HV hv = pl.HV("c");
        is( hv["Mary"].c_str(), "femail" );
        is( hv["Tom"].c_str(), "mail" );
    }

    summary();

    return 0;
}

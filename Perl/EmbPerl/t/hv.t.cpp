//: hv.t.cpp
//: test the Perl::HV class for the Perl::Interp library
//: Copyright (c) 2004-2006 Agent Zhang
//: 2004-03-23 2006-04-10

#include <stdio.h>
#include <embperl.h>

#include <cpptest.h>

int main( int argc, char* argv[], char* env[] ){
	test_plan(6);

	{
		Perl::Interp pl;

		pl.eval( "%a = (dog => 32, cat => 5);" ); 
		Perl::HV hv = pl.HV("a");

        is( hv.fetch("cat").c_str(), "5" );
        is( hv["cat"].c_str(), "5" );

        is( (int)hv.fetch("dog"), 32 );
        is( (int)hv["dog"], 32 );
        
        ok( ! hv.fetch("blah").defined() );
        hv["blah"] = "abc";
        is( hv["blah"].c_str(), "abc" );
    }

    summary();

    return 0;
}

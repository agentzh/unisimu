//: hv.t.cpp
//: test the Perl::HV class for the Perl::Interp library
//: Copyright (c) 2004-2006 Agent Zhang
//: 2004-03-23 2006-04-10

#include <embperl.h>
#include <vector>
#include <algorithm>
#include <cpptest.h>

int main( int argc, char* argv[], char* env[] ){

    test_plan(13);

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

        Perl::HV::iterator it = hv.getIterator();

        std::vector<std::string> keys;

        while( it.moveNext() ) {
            keys.push_back( it.curKey() );
            is( it.curVal().c_str(), hv[it.curKey()].c_str() );
        }

        std::sort( keys.begin(), keys.end() );
        
        is( keys.size(), 3 );
        is( keys[0].c_str(), "blah" );
        is( keys[1].c_str(), "cat" );
        is( keys[2].c_str(), "dog" );
    }

    summary();

    return 0;
}

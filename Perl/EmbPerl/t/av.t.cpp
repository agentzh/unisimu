//: av.t.cpp
//: test the Perl::AV class in the embperl library
//: Copyright (c) 2004-2006 Agent Zhang
//: 2004-03-23 2006-04-10

#include <embperl.h>
#include <cpptest.h>

int main( int argc, char* argv[], char* env[] ){

    test_plan(11);

	{
		Perl::Interp pl;

		// test the Perl::AV class :
		pl.eval( "@a = (2,3,4);" ); 
		Perl::AV av = pl.AV("a");
		ok( av.store(1, pl.eval("5;")) );
		
		// pl.eval( "print \"@a\\n\";",  2 5 4
		
		int elem = av.fetch(0);
		is( elem, 2 );
		// is( av.store(-1,pl.eval("3;")), 0

		elem = av[2];
		is( elem, 4 );
		is( av.length(), 3 );

		av.push( pl.eval("6;") );
		is( av.length(), 4 );
		// pl.eval( "print \"@a\\n\";", 2 5 4 6

		elem = av.pop();
		is(elem, 6);
		is(av.length(), 3);

		pl.eval( "@b=(-1,0);" );
		Perl::AV list = pl.AV("b");
		av.unshift(list);
		// pl.eval( "print \"@a\\n\";", -1 0 2 5 4
		elem = av.shift();
		is(elem, -1);

		av.remove(1);
		// pl.eval( "print \"@a\\n\";", -1 2 5 4
		
		av.growTo(50);
		is(av.length(), 4);
		
		av.clear();
		is(av.length(), 0);
		
		av.undef();
		is(av.length(), 0);
	}

    summary();

    return 0;
}

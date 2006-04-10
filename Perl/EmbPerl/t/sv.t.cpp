//: sv.t.cpp
//: test the Perl::SV class in the embperl library
//: Copyright (c) 2004-2006 Agent Zhang
//: 2004-03-23 2006-04-10

#include <embperl.h>
#include <cpptest.h>

void type_check(Perl::SV sv, int is_int, int is_double, int is_string, int is_ref) {
    is_( sv.isInt(), is_int, "is int" );
    is_( sv.isDouble(), is_double, "is double" );
    is_( sv.isString(), is_string, "is string" );
    is_( sv.isRef(), is_ref, "is ref" );
}

#define IsTrue(val, ans) \
    { \
        Perl::SV sv = (val); \
        is_( sv.isTrue(), (ans), "is true" ); \
    }

int main( int argc, char* argv[], char* env[] ){

    test_plan(58);

    {   // test the Perl::SV class:
        Perl::Interp pl;

        Perl::SV sv = pl.eval("$a='abc';$a =~ m/(\\w{2})/;$b=$1;");
        ok( sv.defined() );
        is( pl.SV("b").c_str(), "ab" );

        //pl.eval("print \"$&\\n\";");
        //pl.eval("$b = get1();");
        Perl::SV sv2 = pl.SV("a");
        is( sv2.c_str(), "abc" );

        sv = pl.eval( "$c = 35;" );
        is( (int)sv, 35 );
        type_check(sv, 1, 0, 0, 0);

        sv = 3.14;
        type_check(sv, 0, 1, 0, 0);

        is( sv.c_str(), "3.14" );
        type_check(sv, 0, 1, 1, 0);

        sv = "abc";
        type_check(sv, 0, 0, 1, 0);

        sv = "31";
        type_check(sv, 0, 0, 1, 0);

        int a = (int)sv;
        is(a, 31);
        type_check(sv, 1, 0, 1, 0);

        sv = pl.eval("@a=(2,3);$ra=\\@a;");
        type_check(sv, 0, 0, 0, 1);

        sv = 4;
        is( (int)sv , 4 );
        ok( sv.defined() );
        sv.undef();
        ok( ! sv.defined() );

        IsTrue(3, 1);
        IsTrue(3.14, 1);
        IsTrue("abc", 1);
        IsTrue(0, 0);
        IsTrue(0.00, 0);
        IsTrue( pl.eval("undef;") , 0 );
        IsTrue( pl.eval("\\@ARGV;") , 1 );

        sv = pl.eval( "require 'FileHandle.pm';" );
        ok( sv.defined() );

        sv = pl.eval( "require 'NoSuchModule.pm';" );
        ok( ! sv.defined() );

        // sv = pl.eval( "require 'Agent2002\\WordPerl.pm';" );
        // printf( "%d\n", (int)sv , 1
    }
    {
        //test Perl::SV::operator= :
        Perl::Interp pl;

        Perl::SV sv = pl.SV("a");
        pl.eval("$b=4;");
        sv = pl.SV("b");
        is( (int)sv, 4 );

        sv = 19;
        is( (int)sv, 19 );
        is( (int)pl.SV("b"), 4 );
        sv.undef();
        ok( ! sv.defined() );
    }
    {
        // test the Reference Counter:
        Perl::Interp pl;

        Perl::SV sv = pl.eval("1+2;");
        is( sv.getRefCount(), 1 )

        sv = pl.eval( "$a = 6;" );
        is( sv.getRefCount(), 1 )

        Perl::SV sv2 = pl.SV("a");
        is( sv2.getRefCount(), 1 );

        pl.eval( "$ra = \\$a;" );
        is( sv2.getRefCount(), 4 );

        sv2.decRefCount();
        is( sv2.getRefCount(), 3 );

        sv2.incRefCount();
        is( sv2.getRefCount(), 4 );

        sv2.toTemp();
        is( sv2.getRefCount(), 4 );

        sv2.undef();
        is( sv2.getRefCount(), 4 );
    }

    summary();

    return 0;
}

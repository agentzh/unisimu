//: plinterp.cpp
//: implementation for the Perl::Interp class
//: v0.03
//: Agent2002. All rights reserved.
//: 04-04-17 04-04-17

#include "embperl.h"

#include <string>
#include <stdio.h>
//#include <eprintf.h>

#include "plproxy.hpp"

extern "C" PerlInterpreter* my_perl;
extern "C" void xs_init( PerlInterpreter* );
PerlInterpreter* get_perl_ptr();

using namespace std;

namespace Perl {

	SV Interp::eval( const string& code ){
		void* c_sv_ptr = Perl_eval_pv( my_perl,
								code.c_str(),
								0 );
		return Perl::SV::SV( c_sv_ptr, 0 );
	}
	
	Perl::SV Interp::SV( const string& name ){
		if( my_perl == NULL )
			my_perl = get_perl_ptr();

		void* c_sv_ptr = Perl_get_sv( my_perl, name.c_str(), 1 );
		return Perl::SV::SV( c_sv_ptr, 0 );
	}

	Perl::AV Interp::AV( const string& name ){
		if( my_perl == NULL )
			my_perl = get_perl_ptr();

		void* c_av_ptr = Perl_get_av( my_perl, name.c_str(), 1 );
		return Perl::AV::AV( c_av_ptr );
	}

	Perl::HV Interp::HV( const string& name ){
		if( my_perl == NULL )
			my_perl = get_perl_ptr();

		void* c_hv_ptr = Perl_get_hv( my_perl, name.c_str(), 1 );
		return Perl::HV::HV( c_hv_ptr );
	}

	// force the Perl interpreter to initialize:
	void Interp::init(){}

	void* Interp::m_c_interp_ptr = get_perl_ptr();

}  // namespace Perl

PerlInterpreter* get_perl_ptr(){
	if( my_perl != NULL )
		return my_perl;
	static char* argv[] = { "", "-w -e", "" };
	static const int argc = 3;

	if( (my_perl = perl_alloc()) == NULL )
		return NULL;

	perl_construct(my_perl);
	perl_parse( my_perl, xs_init, argc, argv, NULL );

	return my_perl;
}

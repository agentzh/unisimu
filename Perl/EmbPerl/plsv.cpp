//: plsv.cpp
//: implementation for the Perl::SV class
//: v0.03
//: Agent2002. All rights reserved.
//: 04-04-17 04-04-17

#include "embperl.h"
#include "plproxy.hpp"

//#include <eprintf.h>

typedef SV* old_SV_ptr;
extern "C" PerlInterpreter* my_perl;

namespace Perl {

	SV::SV(){
		// initialize the Perl interpreter:
		Interp::init();

		m_c_sv_ptr = Perl_newSVsv( my_perl, proxy_sv_undef_ptr() );
	}

	SV::SV( const SV& val ){
		Interp::init();
		m_c_sv_ptr = val.m_c_sv_ptr;
	}

	SV::SV( const int val ){
		Interp::init();
		m_c_sv_ptr = Perl_newSVsv( my_perl, proxy_sv_undef_ptr() );
		Perl_sv_setiv( my_perl, (old_SV_ptr)m_c_sv_ptr, val );
	}

	SV::SV( const double val ){
		Interp::init();
		m_c_sv_ptr = Perl_newSVsv( my_perl, proxy_sv_undef_ptr() );
		Perl_sv_setnv( my_perl, (old_SV_ptr)m_c_sv_ptr, val );
	}

	SV::SV( const string& val ){
		Interp::init();
		m_c_sv_ptr = Perl_newSVsv( my_perl, proxy_sv_undef_ptr() );
		Perl_sv_setpv( my_perl, (old_SV_ptr)m_c_sv_ptr, val.c_str() );
	}

	SV::SV( const char* val ){
		Interp::init();
		m_c_sv_ptr = Perl_newSVsv( my_perl, proxy_sv_undef_ptr() );
		Perl_sv_setpv( my_perl, (old_SV_ptr)m_c_sv_ptr, val );
	}

	SV& SV::operator=( const SV& right ){
		SV temp = right;
		if( this == &temp )
			return *this;

		Perl_sv_setsv_flags( my_perl,
							(old_SV_ptr)m_c_sv_ptr,
							(old_SV_ptr)temp.m_c_sv_ptr, 0 );
		return *this;
	}

	bool SV::isInt(){
		return (bool) proxy_SvIOK( (old_SV_ptr)m_c_sv_ptr );
	}

	bool SV::isDouble(){
		return (bool) proxy_SvNOK( (old_SV_ptr)m_c_sv_ptr );
	}

	bool SV::isString(){
		return (bool) proxy_SvPOK( (old_SV_ptr)m_c_sv_ptr );
	}

	bool SV::isRef(){
		return (bool) proxy_SvROK( (old_SV_ptr)m_c_sv_ptr );
	}

	bool SV::defined(){
		return (bool) proxy_SvOK( (old_SV_ptr)m_c_sv_ptr );
	}

	bool SV::isTrue(){
		return (bool) proxy_SvTRUE( (old_SV_ptr)m_c_sv_ptr );
	}

	SV::operator int(){
		return proxy_SvIV( (old_SV_ptr)m_c_sv_ptr );
	}

	SV::operator double(){
		return proxy_SvNV( (old_SV_ptr)m_c_sv_ptr );
	}

	SV::operator string(){
		return proxy_SvPV_nolen( (old_SV_ptr)m_c_sv_ptr );
	}

    const char* SV::c_str() {
		return proxy_SvPV_nolen( (old_SV_ptr)m_c_sv_ptr );
    }

	void SV::undef(){
		Perl_sv_setsv_flags( my_perl,
							(old_SV_ptr)m_c_sv_ptr,
							proxy_sv_undef_ptr(),
							0 );
	}

	unsigned int SV::getRefCount(){
		return proxy_SvREFCNT( (old_SV_ptr)m_c_sv_ptr );
	}

	void SV::decRefCount(){
		proxy_SvREFCNT_dec( (old_SV_ptr)m_c_sv_ptr );
	}

	void SV::incRefCount(){
		m_c_sv_ptr = proxy_SvREFCNT_inc( (old_SV_ptr)m_c_sv_ptr );
	}

	void SV::toTemp(){
		m_c_sv_ptr = Perl_sv_2mortal( my_perl,
									  (old_SV_ptr)m_c_sv_ptr );
	}

	ostream& operator<<( ostream& os, const SV& sv ){
		SV temp = sv;
		return os << (string)temp;
	}

	SV::SV( void* c_sv_ptr, int ){
		Interp::init();
		m_c_sv_ptr = c_sv_ptr;
	}

}  // namespace Perl

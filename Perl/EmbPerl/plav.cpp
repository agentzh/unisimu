//: plav.cpp
//: implementation for the Perl::AV class
//: v0.03
//: Agent2002. All rights reserved.
//: 04-04-17 04-04-28

#include "embperl.h"
#include "plproxy.hpp"

//#include <eprintf.h>

typedef SV* old_SV_ptr;
typedef AV* old_AV_ptr;
extern "C" PerlInterpreter* my_perl;

namespace Perl {

	AV::AV(){
		// initialize the Perl interpreter:
		Interp::init();

		m_c_av_ptr = Perl_newAV( my_perl );
	}

	AV::AV( const AV& val ){
		m_c_av_ptr = val.m_c_av_ptr;
	}

	bool AV::store( int index, const SV& val ){
		Perl::SV temp = val;
		temp.incRefCount();
		old_SV_ptr* pp = Perl_av_store( my_perl,
								(old_AV_ptr)m_c_av_ptr,
								index,
								(old_SV_ptr)temp.m_c_sv_ptr );
		if( pp == NULL ){
			temp.decRefCount();
			return false;
		}
		return true;
	}

	SV AV::fetch( int index ){
		old_SV_ptr* pp = Perl_av_fetch( my_perl,
										(old_AV_ptr)m_c_av_ptr,
										index,
										0 );
		if( pp == NULL ){
			SV sv;
			sv = SV( proxy_sv_undef_ptr(), 0 );
			store( index, sv );
			return sv;
		}
		return SV( *pp, 0 );
	}

	SV AV::operator[]( int index ){
		return fetch( index );
	}

	int AV::length(){
		return Perl_av_len( my_perl,(old_AV_ptr)m_c_av_ptr ) + 1;
	}

    void AV::push( const SV& val ){
		SV temp = val;
		Perl_av_push( my_perl,
					 (old_AV_ptr)m_c_av_ptr,
					 (old_SV_ptr)temp.m_c_sv_ptr );
	}

	SV AV::pop(){
		void* c_sv_ptr = Perl_av_pop( my_perl,
									   (old_AV_ptr)m_c_av_ptr );
		return SV( c_sv_ptr, 0 );
	}

	void AV::unshift( const AV& list ){
		AV array = list;
		int len = array.length();
		Perl_av_unshift( my_perl,(old_AV_ptr)m_c_av_ptr, len );
		for( int i = 0; i < len; i++ )
			store( i, array[i] );
	}

	SV AV::shift(){
		void* c_sv_ptr = Perl_av_shift( my_perl,
										(old_AV_ptr)m_c_av_ptr );
		return SV( c_sv_ptr, 0 );
	}

	SV AV::remove( int index ){
		void* c_sv_ptr = Perl_av_delete( my_perl,
										  (old_AV_ptr)m_c_av_ptr,
										  index,
										  0 );
		return SV( c_sv_ptr, 0 );
	}

	void AV::undef(){
		Perl_av_undef( my_perl,(old_AV_ptr)m_c_av_ptr );
	}

	void AV::clear(){
		Perl_av_clear( my_perl,(old_AV_ptr)m_c_av_ptr );
	}

	void AV::growTo( int index ){
		Perl_av_extend( my_perl,(old_AV_ptr)m_c_av_ptr, index );
	}

	ostream& operator<<( ostream& os, const AV& av ){
		AV temp = av;

		int len = temp.length();
		if( len == 0 )
			return os << "()";

		string s = "(";
		for( int i = 0; i < len - 1; i++ )
			s += (string)temp[i] + ",";
		s += (string)temp[len-1] + ")";

		return os << s;
	}

	AV::AV( void* c_av_ptr ){
		Interp::init();
		m_c_av_ptr = c_av_ptr;
	}

}  // namespace Perl

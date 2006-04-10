//: plhv.cpp
//: implementation for the Perl::HV class
//: v0.03
//: Agent2002. All rights reserved.
//: 04-04-17 04-04-17

#include "embperl.h"
#include "plproxy.hpp"

#include <string.h>
//#include <eprintf.h>

typedef SV* old_SV_ptr;
typedef HV* old_HV_ptr;
typedef HE* old_HE_ptr;

extern "C" PerlInterpreter* my_perl;

namespace Perl {

	HV::HV(){
		// initialize the Perl interpreter:
		Interp::init();

		m_c_hv_ptr = Perl_newHV( my_perl );
	}

	HV::HV( const HV& val ){
		m_c_hv_ptr = val.m_c_hv_ptr;
	}

	bool HV::store( const string& key, const SV& val ){
		SV temp = val;
		old_SV_ptr* pp = Perl_hv_store( my_perl,
										(old_HV_ptr)m_c_hv_ptr,
										key.c_str(),
										strlen( key.c_str() ),
										(old_SV_ptr)temp.m_c_sv_ptr,
										0 );
		if( pp == NULL ){
			temp.decRefCount();
			return false;
		}
		return true;
	}

	SV HV::fetch( const string& key ){
		old_SV_ptr* pp = Perl_hv_fetch( my_perl,
										(old_HV_ptr)m_c_hv_ptr,
										key.c_str(),
										strlen( key.c_str() ),
										0 );
		if( pp == NULL ){
			SV sv;
			sv = SV( proxy_sv_undef_ptr(), 0 );
			store( key, sv );
			return sv;
		}
		return SV( *pp, 0 );
	}

	SV HV::operator[]( const string& key ){
		return fetch(key);
	}

	SV HV::remove( const string& key ){
		SV retval( Perl_hv_delete( my_perl,
								   (old_HV_ptr)m_c_hv_ptr,
								   key.c_str(),
								   strlen( key.c_str() ),
								   0 ),
					0 );
		return retval;
	}

	void HV::undef(){
		Perl_hv_undef( my_perl, (old_HV_ptr)m_c_hv_ptr );
	}

	void HV::clear(){
		Perl_hv_clear( my_perl, (old_HV_ptr)m_c_hv_ptr );
	}

	int HV::iterator::count(){
		return m_count;
	}

	bool HV::iterator::moveNext(){
		void* p = Perl_hv_iternext( my_perl, (old_HV_ptr)m_table );
		if( p == NULL )
			return false;
		m_entry = p;
		return true;
	}

	SV HV::iterator::curVal(){
		void* c_sv_ptr = Perl_hv_iterval( my_perl, 
										  (old_HV_ptr)m_table,
										  (old_HE_ptr)m_entry );
		return HV::newSV( c_sv_ptr );
	}

	string HV::iterator::curKey( int* retlen ){
		if( retlen == NULL ){
			int len;
			char* retval = Perl_hv_iterkey( my_perl,
											(old_HE_ptr)m_entry,
											&len );
			if( retval[len] != '\0' )
				return NULL;
			return retval;
		}
		return Perl_hv_iterkey( my_perl,
								(old_HE_ptr)m_entry,
								retlen );
	}

	HV::iterator::iterator( void* table, int count ){
		m_table = table;
		m_count = count;
		m_entry = NULL;
	}

	HV::iterator HV::getIterator(){
		int count = Perl_hv_iterinit( my_perl,
									  (old_HV_ptr)m_c_hv_ptr );
		return HV::iterator( m_c_hv_ptr, count );
	}

	ostream& operator<<( ostream& os, const HV& hv ){
		HV temp = hv;
		HV::iterator it = temp.getIterator();
		
		int len = it.count();
		if( len == 0 )
			return os << "()";
		
		string s = "(";
		for( int i = 0; i < len - 1; i++ ){
			it.moveNext();
			s += (string)it.curKey() + "=>" + (string)it.curVal();
			s += ",";
		}
		it.moveNext();
		s += (string)it.curKey() + "=>" + (string)it.curVal();
		s += ")";
	
		return os << s;
	}

	HV::HV( void* c_hv_ptr ){
		Interp::init();
		m_c_hv_ptr = c_hv_ptr;
	}

	SV HV::newSV( void* c_sv_ptr ){
		return SV( c_sv_ptr, 0 );
	}
}  // namespace Perl

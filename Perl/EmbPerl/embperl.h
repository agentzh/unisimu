//: embperl.h
//: Header for the embperl Library
//: Copyright (c) 2004-2006 Agent Zhang
//: 2004-04-17 2006-04-10

#ifndef EMBPERL_H_
#define EMBPERL_H_

#include <string>
#include <iostream>

namespace Perl {
	using namespace std;

	class Interp;
	class AV;
	class HV;

	// SV: class for Scalar Variables in Perl
	class SV {
		friend class Interp;
		friend class AV;
		friend class HV;

	public:
		SV();

		SV( const int val );
		SV( const double val );
		SV( const string& val );
		SV( const char* val );

		// pass the brute SV pointer directly
		SV( const SV& val );

		// copy the brute SV pointer
		SV& operator=( const SV& right );

		bool isInt();
		bool isDouble();
		bool isString();
		bool isRef();

		bool defined();
		bool isTrue();

		operator int();
		operator double();
		operator string();

        const char* c_str();

		void undef();

		unsigned int getRefCount();
		void decRefCount();
		void incRefCount();
		void toTemp();

		friend ostream& operator<<( ostream& os, const SV& sv );
	private:
		explicit SV( void* c_sv_ptr, int );

	private:
		// brute SV pointer in the C language:
		void* m_c_sv_ptr;

	};

	// AV: class for Array Variables in Perl
	class AV {
		friend class Interp;
	public:
		AV();
		AV( const AV& val );

		bool store( int index, const SV& elem );
		SV fetch( int index );
		SV operator[]( int index );
		int length();

		void push( const SV& elem );
		SV pop();

		void unshift( const AV& list );
		SV shift();

		void growTo( int index );
		SV remove( int index );
		void clear();
		void undef();

		friend ostream& operator<<( ostream& os, const AV& av );
	private:
		explicit AV( void* c_av_ptr );

	private:
		// brute AV pointer in the C language:
		void* m_c_av_ptr;
	};

	// HV: class for Hash Variables in Perl
	class HV {
		friend class Interp;
	public:
		HV();
		HV( const HV& val );

		bool store( const string&  key, const SV& val );
		SV fetch( const string& key );
		SV operator[]( const string& key );

		SV remove( const string& key );
		void clear();
		void undef();
		
		class iterator;
		friend class iterator;

		class iterator{
		public:
			friend class Perl::HV;
		public:
			int count();
			bool moveNext();

			SV curVal();
			string curKey( int* retlen = NULL );

		private:
			iterator( void* table, int count );

			void* m_table;
			void* m_entry;
			int m_count;
		};
		
		iterator getIterator();
		friend ostream& operator<<( ostream& os, const HV& hv );

	private:
		explicit HV( void* c_hv_ptr );
		static SV newSV( void* c_sv_ptr );

	private:
		// brute HV pointer in the C language:
		void* m_c_hv_ptr;
	};

	// Interp: class for the Perl Interpreter
	class Interp {
		friend class SV;
		friend class AV;
		friend class HV;
	public:
		static SV eval( const string& cmd );

		static Perl::SV SV( const string& name );
		static Perl::AV AV( const string& name );
		static Perl::HV HV( const string& name );

	private:
		static void init();

		// brute PerlInterpreter pointer in the C language:
		static void* m_c_interp_ptr;
	};
}

#endif  // EMBPERL_H_

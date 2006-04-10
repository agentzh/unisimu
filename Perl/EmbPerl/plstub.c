//: plstub.c
//: Stub for Perl API's
//: v0.03
//: Agent2002. All rights reserved.
//: 04-03-25 04-04-17

#include <EXTERN.h>
#include <perl.h>

PerlInterpreter* my_perl = NULL;

// definitions of functions wrapping the Perl API macros:
I32 proxy_SvREFCNT( SV* sv ){
	return SvREFCNT(sv);
}
void proxy_SvREFCNT_dec( SV* sv ){
	SvREFCNT_dec(sv);
}
SV* proxy_SvREFCNT_inc( SV* sv ){
	return SvREFCNT_inc(sv);
}
U32 proxy_SvIOK(SV* sv){
	return SvIOK(sv);
}
IV proxy_SvIV(SV* sv){
	return SvIV(sv);
}
U32 proxy_SvNOK(SV* sv){
	return SvNOK(sv);
}
NV proxy_SvNV(SV* sv){
	return SvNV(sv);
}
U32 proxy_SvOK(SV* sv){
	return SvOK(sv);
}
U32 proxy_SvPOK(SV* sv){
	return SvPOK(sv);
}
char* proxy_SvPV_nolen(SV* sv){
	return SvPV_nolen(sv);
}
U32 proxy_SvROK(SV* sv){
	return SvROK(sv);
}	
U32 proxy_SvTRUE(SV* sv){
	return SvTRUE(sv);
}

// definitions of variables wrapping the Perl macro constants:
SV* proxy_sv_no_ptr( void ){
	return &PL_sv_no;
}
SV* proxy_sv_undef_ptr( void ){
	return &PL_sv_undef;
}
SV* proxy_sv_yes_ptr( void ){
	return &PL_sv_yes;
}


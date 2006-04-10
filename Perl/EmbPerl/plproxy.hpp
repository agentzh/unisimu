//: plproxy.hpp
//: Proxy for Perl API's
//: v0.03
//: Agent2002. All rights reserved.
//: 04-03-25 04-04-24

#ifndef PLPROXY_HPP_
#define PLPROXY_HPP_

#include <stdlib.h>

// type declarations:
typedef int I32;
typedef unsigned int U32;
typedef size_t STRLEN;
typedef long IV;
typedef double NV;
typedef unsigned long UV;

struct PerlInterpreter;
typedef void (*XSINIT_t) (PerlInterpreter*);
extern "C" {
struct AV;
struct SV;
struct HV;
struct CV;
struct GV;
struct HE;
struct SP;
struct IO;
struct ST;
};

// declarations of C functions in Pel API's:
extern "C" {

PerlInterpreter*        perl_alloc();
void    perl_construct(PerlInterpreter* interp);
int     perl_destruct(PerlInterpreter* interp);
void    perl_free(PerlInterpreter* interp);
int     perl_parse(PerlInterpreter* interp, XSINIT_t xsinit, int argc, char** argv, char** env);
int     perl_run(PerlInterpreter* interp);

AV*	Perl_newAV(register PerlInterpreter *my_perl );
void	Perl_av_clear(register PerlInterpreter *my_perl , AV* ar);
SV*	Perl_av_delete(register PerlInterpreter *my_perl , AV* ar, I32 key, I32 flags);
void	Perl_av_extend(register PerlInterpreter *my_perl , AV* ar, I32 key);
SV**	Perl_av_fetch(register PerlInterpreter *my_perl , AV* ar, I32 key, I32 lval);
I32	Perl_av_len(register PerlInterpreter *my_perl , AV* ar);
SV*	Perl_av_pop(register PerlInterpreter *my_perl , AV* ar);
void	Perl_av_push(register PerlInterpreter *my_perl , AV* ar, SV* val);
SV*	Perl_av_shift(register PerlInterpreter *my_perl , AV* ar);
SV**	Perl_av_store(register PerlInterpreter *my_perl , AV* ar, I32 key, SV* val);
void	Perl_av_undef(register PerlInterpreter *my_perl , AV* ar);
void	Perl_av_unshift(register PerlInterpreter *my_perl , AV* ar, I32 num);


HV*	Perl_newHV(register PerlInterpreter *my_perl );
void	Perl_hv_clear(register PerlInterpreter *my_perl , HV* tb);
SV*	Perl_hv_delete(register PerlInterpreter *my_perl , HV* tb, const char* key, I32 klen, I32 flags);
SV**	Perl_hv_fetch(register PerlInterpreter *my_perl , HV* tb, const char* key, I32 klen, I32 lval);
I32	Perl_hv_iterinit(register PerlInterpreter *my_perl , HV* tb);
char*	Perl_hv_iterkey(register PerlInterpreter *my_perl , HE* entry, I32* retlen);
HE*	Perl_hv_iternext(register PerlInterpreter *my_perl , HV* tb);
SV*	Perl_hv_iterval(register PerlInterpreter *my_perl , HV* tb, HE* entry);
SV**	Perl_hv_store(register PerlInterpreter *my_perl , HV* tb, const char* key, I32 klen, SV* val, U32 hash);
void	Perl_hv_undef(register PerlInterpreter *my_perl , HV* tb);
SV*	Perl_eval_pv(register PerlInterpreter *my_perl , const char* p, I32 croak_on_error);

SV*	Perl_get_sv(register PerlInterpreter *my_perl , const char* name, I32 create);
AV*	Perl_get_av(register PerlInterpreter *my_perl , const char* name, I32 create);
HV*	Perl_get_hv(register PerlInterpreter *my_perl , const char* name, I32 create);

SV*	Perl_sv_2mortal(register PerlInterpreter *my_perl , SV* sv);
SV*	Perl_newSVsv(register PerlInterpreter *my_perl , SV* old);
void	Perl_sv_setiv(register PerlInterpreter *my_perl , SV* sv, IV num);
void	Perl_sv_setnv(register PerlInterpreter *my_perl , SV* sv, NV num);
void	Perl_sv_setpv(register PerlInterpreter *my_perl , SV* sv, const char* ptr);
void	Perl_sv_setsv_flags(register PerlInterpreter *my_perl , SV* dsv, SV* ssv, I32 flags);

}

// declarations of functions wrapping the Perl API macros:
extern "C" {

U32 proxy_SvREFCNT( SV* sv );
void proxy_SvREFCNT_dec( SV* sv );
SV* proxy_SvREFCNT_inc( SV* sv );

U32    proxy_SvIOK(SV* sv);
IV      proxy_SvIV(SV* sv);
U32    proxy_SvNOK(SV* sv);
NV      proxy_SvNV(SV* sv);
U32    proxy_SvOK(SV* sv);
U32    proxy_SvPOK(SV* sv);
char*   proxy_SvPV_nolen(SV* sv);
U32    proxy_SvROK(SV* sv);
U32    proxy_SvTRUE(SV* sv);
}

// declarations of variables wrapping the Perl macro constants:
extern "C" {
SV* proxy_sv_no_ptr();
SV* proxy_sv_undef_ptr();
SV* proxy_sv_yes_ptr();

}

#endif	// PLPROXY_HPP_

/*************************************************/
/**: eprintf.h	- header file for error wrapper **/
/**:       functions                            **/
/**: picked from "The Practice of Programming"  **/
/**: All rights reserved.                       **/
/**: - 12/21/2003   08/02/2004  Perl_Y 31A 32B  **/
/*************************************************/

#ifndef _EPRINTF_H_
#define _EPRINTF_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>

#define efree(x)    free(x)

#ifdef __cplusplus
extern "C"{
#endif

char* progname() {
    return NULL;
}

/* eprintf: print error message and exit */
void eprintf(char* fmt, ...){
    va_list args;

    fflush(stdout);
    if (progname() != NULL)
        fprintf(stderr, "%s: ", progname());

    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);

    if (fmt[0] != '\0' && fmt[strlen(fmt)-1] == ':'){
        fprintf(stderr, " %s\n", strerror(errno));
    }
    exit(2);    /* conventional value for failed execution */
}

/* weprintf: print warning message without exit */
void weprintf(char* fmt, ...){
    va_list args;

    fflush(stdout);
    if (progname() != NULL)
        fprintf(stderr, "%s: ", progname());

    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);

    if (fmt[0] != '\0' && fmt[strlen(fmt)-1] == ':'){
        fprintf(stderr, " %s", strerror(errno));
        fprintf(stderr, "\n");
    }
}

/* estrdup: duplicate a string, report if error */
char* estrdup(char* s){
    char* t;
    t = (char*) malloc(strlen(s)+1);
    if (t == NULL)
        eprintf("estrdup(\"%.20s\") failed:", s);
    strcpy(t, s);
    return t;
}

/* emalloc: malloc and report if error */
void* emalloc(size_t n){
    void* p;
    
    p = malloc(n);
    if (p == NULL)
        eprintf("malloc of %u bytes failed:", n);
    return p;
}

/* erealloc: realloc and report if error */
void* erealloc(void* s, size_t n){
    void* p;
    
    p = realloc(s, n);
    if (p == NULL)
        eprintf("realloc of %u bytes failed:", n);
    return p;
}

static char* name = NULL;   /* program name for messages */

#ifdef __cplusplus
}
#endif

#define GOT_HERE \
	weprintf( "%s (line %d) : GOT HERE!\n", __FILE__, __LINE__ );

#endif  /* _EPRINTF_H_ */

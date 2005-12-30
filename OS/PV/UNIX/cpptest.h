//: cpptest.h
//: C++ port for Perl's Test::More module
//: v0.01
//: Copyright (c) Agent Zhang
//: 2005-10-13 2005-10-15

#ifndef _CPPTEST_H_
#define _CPPTEST_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


#define ok(a) \
    Ok(__FILE__, __LINE__, a, "");

#define ok_(a, info) \
    Ok(__FILE__, __LINE__, a, info);

#define is(a,b) \
    Is(__FILE__, __LINE__, a, b, "");

#define is_(a,b,info) \
    Is(__FILE__, __LINE__, a, b, info);

#define isnt(a,b) \
    Isnt(__FILE__, __LINE__, a, b, "");

#define isnt_(a,b,info) \
    Isnt(__FILE__, __LINE__, a, b, info);

#define FLOAT_EPS 1.0e-6
#define DOUBLE_EPS 1.0e-15
long Count = 0;
long Total = 0;
long ErrorCount = 0;
long SkipCount = 0;
long TodoCount = 0;

//Test functions statement 
void test_plan(long total);

void summary(void);

void Ok(const char* fname, long lineno, int a, const char* info); 

void Is(const char* fname, long lineno,
        const char* a, const char* b, const char* info);
void Is(const char* fname, long lineno, 
        const double a, const double b, const char* info);
void Is(const char* fname, long lineno, 
        const float a, const float b, const char* info);
void Is(const char* fname, long lineno, 
        const int a, const int b, const char* info);
void Is(const char* fname, long lineno, 
        const short a, const short b, const char* info);
void Is(const char* fname, long lineno, 
        const long a, const long b, const char* info);

void Isnt(const char* fname, long lineno, 
          const char* a, const char* b, const char* info);        
void Isnt(const char* fname, long lineno, 
          const double a, const double b, const char* info);
void Isnt(const char* fname, long lineno, 
          const float a, const float b,   const char* info);
void Isnt(const char* fname, long lineno, 
          const int a, const int b,  const char* info);
void Isnt(const char* fname, long lineno, 
          const short a, const short b, const char* info);
void Isnt(const char* fname, long lineno, 
          const long a, const long b, const char* info);


void test_plan(long total) {
    Total = total;
    printf("1..%d\n", Total);
    fflush(stdout);
}

void summary(void) {
    if (Count < Total) {
        fprintf(stderr, "# Looks like you planned %ld tests but only ran %ld.\n",
            Total, Count);
    } else if (Count > Total) {
        if (Total == 1)
            fprintf(stderr, "# Looks like you planned 1 test but ran %ld extra.\n", Count - Total);
        else
            fprintf(stderr, "# Looks like you planned %ld tests but ran %ld extra.\n", Total, Count - Total);
    } else {
        if (ErrorCount == 1) {
            fprintf(stderr, "# Looks like you failed 1 test of %ld.\n", Total);
            return;
        }
        if (ErrorCount > 1) {
            fprintf(stderr, "# Looks like you failed %ld tests of %ld.\n", ErrorCount, Total);
            return;
        }
    }
}

void Ok(const char* fname, long lineno, int a, const char* info) {
    ++Count;
    if (a) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d\n", Count);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n",
            fname, lineno);
        fflush(stderr);
    }
}

void Ok(const char* fname, long lineno, void* ptr, const char* info) {
    ++Count;
    if (ptr) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d\n", Count);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n",
            fname, lineno);
        fflush(stderr);
    }
}

// Is char
void Is(const char* fname, long lineno,
        const char* a, const char* b, const char* info) {
    ++Count;
    if (strcmp(a, b) == 0) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d - %s\n", Count, info);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n"
               "#          got: '%s'\n"
               "#     expected: '%s'\n",
               fname, lineno, a, b);
        fflush(stderr);
    }
}


//Is int
void Is(const char* fname, long lineno,
        const int a, const int b, const char* info) {
    ++Count;
    if (a == b) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d - %s\n", Count, info);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n"
               "#     '%d'\n"
               "#         ne\n"
               "#     '%d'\n",
               fname, lineno, a, b);
        fflush(stderr);
    }
}

//Is short
void Is(const char* fname, long lineno,
        const short  a, const short  b, const char* info) {
    ++Count;
    if (a == b) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d - %s\n", Count, info);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n"
               "#     '%d'\n"
               "#         ne\n"
               "#     '%d'\n",
               fname, lineno, a, b);
        fflush(stderr);
    }
}

// Is long

void Is(const char* fname, long lineno,
        const long  a, const long  b, const char* info) {
    ++Count;
    if (a == b) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d - %s\n", Count, info);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n"
               "#     '%ld'\n"
               "#         ne\n"
               "#     '%ld'\n",
               fname, lineno, a, b);
        fflush(stderr);
    }
}
//?Is double
void Is(const char* fname, long lineno,
        const double a, const double b, const char* info) {
    ++Count;
    if (a == b ) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d - %s\n", Count, info);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n"
               "#     '%f'\n"
               "#         ne\n"
               "#     '%f'\n",
               fname, lineno, a, b);
        fflush(stderr);
    }
}

// Is float
void Is(const char* fname, long lineno,
        const float a, const float b, const char* info) {
    ++Count;
    if (  a == b ) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d - %s\n", Count, info);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n"
               "#     '%f'\n"
               "#         ne\n"
               "#     '%f'\n",
               fname, lineno, a, b);
        fflush(stderr);
    }
}

//the Isnt
//Isnt char
void Isnt(const char* fname, long lineno,
        const char* a, const char* b, const char* info) {
    ++Count;
    if (strcmp(a, b) != 0) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d - %s\n", Count, info);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n"
               "#     '%s'\n"
               "#         ne\n"
               "#     '%s'\n",
               fname, lineno, a, b);
        fflush(stderr);
    }
}


//Isnt int
void Isnt(const char* fname, long lineno,
        const int a, const int b, const char* info) {
    ++Count;
    if (a != b) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d - %s\n", Count, info);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n"
               "#     '%d'\n"
               "#         ne\n"
               "#     '%d'\n",
               fname, lineno, a, b);
        fflush(stderr);
    }
}

//Isnt short
void Isnt(const char* fname, long lineno,
          const short  a, const short  b, const char* info) {
    ++Count;
    if (a != b) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d - %s\n", Count, info);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n"
               "#     '%d'\n"
               "#         ne\n"
               "#     '%d'\n",
               fname, lineno, a, b);
        fflush(stderr);
    }
}

// Isnt long

void Isnt(const char* fname, long lineno,
          const long  a, const long  b, const char* info) {
    ++Count;
    if (a != b) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d - %s\n", Count, info);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n"
               "#     '%ld'\n"
               "#         ne\n"
               "#     '%ld'\n",
               fname, lineno, a, b);
        fflush(stderr);
    }
}

//?Isnt double
void Isnt(const char* fname, long lineno,
          const double a, const double b, const char* info) {
    ++Count;
    if ( a != b ) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d - %s\n", Count, info);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n"
               "#     '%f'\n"
               "#         ne\n"
               "#     '%f'\n",
               fname, lineno, a, b);
        fflush(stderr);
    }
}

// Isnt float
void Isnt(const char* fname, long lineno,
          const float a, const float b, const char* info) {
    ++Count;
    if ( a != b ) {
        if (strcmp(info, "") == 0)
            printf("ok %d\n", Count);
        else
            printf("ok %d - %s\n", Count, info);
        fflush(stdout);
    } else {
        ErrorCount++;
        if (strcmp(info, "") == 0)
            printf("not ok %d\n", Count);
        else
            printf("not ok %d - %s\n", Count, info);
        fflush(stdout);
        fprintf(stderr, "#     Failed test (%s at line %d)\n"
               "#     '%f'\n"
               "#         ne\n"
               "#     '%f'\n",
               fname, lineno, a, b);
        fflush(stderr);
    }
}


#endif  // _CPPTEST_H_

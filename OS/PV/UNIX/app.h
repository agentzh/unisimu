/******************************************
    app.h
    Copyright (c) 2005 Agent Zhang
    2005-12-29 2005-12-30
 ******************************************/ 

#ifndef _APP_H_
#define _APP_H_

#include "pv.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

enum {
    DEBUG = 0,

    /* user constants */
    PLATE_SZ = 4,
    TIMES    = 20,
    BUFSIZE  = 10,

    SEED = 123456,

    /* shared memeory keys */
    SHM_PLATE = SEED,
    SHM_APPLE_SP = SEED + 1,
    SHM_ORANGE_SP = SEED + 2,

    /* synchronous semaphore keys */
    SEM_CAN_PUT = SEED + 3,
    SEM_CAN_GET_APPLE = SEED + 4,
    SEM_CAN_GET_ORANGE = SEED + 5,

    /* exclusive semaphore keys */
    SEM_APPLE_SP = SEED + 6,
    SEM_ORANGE_SP = SEED + 7,

    /* shared memory sizes */
    SHM_PLATE_SZ = PLATE_SZ * BUFSIZE,
    SHM_APPLE_SP_SZ = sizeof(int),
    SHM_ORANGE_SP_SZ = sizeof(int),
};

/* err: print error message and exit */
void err(char* fmt, ...){
    va_list args;
    fflush(stdout);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    exit(2);
}

#define HERE \
    if (DEBUG) \
        fprintf(stderr, "%s: line %d : GOT HERE!\n", __FILE__, __LINE__);

#endif

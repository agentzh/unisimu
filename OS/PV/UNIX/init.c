/******************************************
    init.c
    Copyright (c) 2005 Agent Zhang
    2005-12-29 2005-12-29
 ******************************************/ 

#include "app.h"

int main() {
    int* orange_sp_ptr;
    int orange_sp;

    /* set up shared memory buffers */
    if (!create_shared_mem(SHM_PLATE, SHM_PLATE_SZ) ) {
        err("Failed to create shared memory 'plate'.");
    }

    if ( !create_shared_mem(SHM_APPLE_SP, SHM_APPLE_SP_SZ) ) {
        err("Failed to create shared memory 'apple_sp'.");
    }

    if ( !create_shared_mem(SHM_ORANGE_SP, SHM_ORANGE_SP_SZ) ) {
        err("Failed to create shared memory 'orange_sp'.");
    }

    orange_sp_ptr = (int*) get_shared_mem(SHM_ORANGE_SP, SHM_ORANGE_SP_SZ);
    if (orange_sp_ptr == NULL) err("Can't get 'orange_sp'.");
    *orange_sp_ptr = PLATE_SZ - 1;
    commit_shared_ptr(orange_sp_ptr);

    /* create semaphores */
    if ( !create_sema(SEM_CAN_PUT, PLATE_SZ) ) {
        err("Failed to create semaphore 'can_put'.");
    }

    if ( !create_sema(SEM_CAN_GET_APPLE, 0) ) {
        err("Failed to create semaphore 'can_get_apple'.");
    }

    if ( !create_sema(SEM_CAN_GET_ORANGE, 0) ) {
        err("Failed to create semaphore 'can_get_orange'.");
    }

    if ( !create_sema(SEM_APPLE_SP, 1) ) {
        err("Failed to create semaphore 'apple_sp'.");
    }

    if ( !create_sema(SEM_ORANGE_SP, 1) ) {
        err("Failed to create semaphore 'orange_sp'.");
    }

    return 0;
}

/*

this program must be run first, or you will get the following error message on Solaris 8:

    ×ÜÏß´íÎó£­ºËÐÄÏÝÚå

And you must clean all the semaphores and shared memory units before running ``init'':

    ./clean

*/

/******************************************
    destroy.c
    Copyright (c) 2005 Agent Zhang
    2005-12-30 2005-12-30
 ******************************************/ 

#include "app.h"

int main(void) {
    /* release shared memory buffers */
    free_shared_mem(SHM_PLATE, SHM_PLATE_SZ);
    free_shared_mem(SHM_APPLE_SP, SHM_APPLE_SP_SZ);
    free_shared_mem(SHM_ORANGE_SP, SHM_ORANGE_SP_SZ);

    /* release semaphores */
    free_sema(SEM_CAN_PUT);
    free_sema(SEM_CAN_GET_APPLE);
    free_sema(SEM_CAN_GET_ORANGE);
    free_sema(SEM_APPLE_SP);
    free_sema(SEM_ORANGE_SP);

    return 0;
}

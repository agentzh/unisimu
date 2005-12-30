/******************************************
    father.c
    Copyright (c) 2005 Agent Zhang
    2005-12-29 2005-12-29
 ******************************************/ 

#include "app.h"
#include <string.h>

/*
    for (1..TIMES) {
        P('can_put');
        P('apple_sp');
        print "Father is putting an apple...\n";
        $plate[$apple_sp] = 'apple';
        $apple_sp++;
        V('apple_sp');
        V('can_get_apple');
    }
*/

void father(void) {
    int* apple_sp_ptr;
    int apple_sp;

    char* plate;

    GOT_HERE

    P(SEM_CAN_PUT);
    P(SEM_APPLE_SP);

    GOT_HERE

    apple_sp_ptr = (int*) get_shared_mem(SHM_APPLE_SP, SHM_APPLE_SP_SZ);
    if (apple_sp_ptr == NULL) err("Can't get 'apple_sp'.");
    apple_sp = *apple_sp_ptr;

    fprintf(stderr, "apple_sp = %d\n", apple_sp);

    GOT_HERE

    plate = (char*) get_shared_mem(SHM_PLATE, SHM_PLATE_SZ);
    if (plate == NULL) err("Can't get 'plate'.");

    GOT_HERE

    printf("Father is putting an apple...\n");
    fflush(stdout);

    GOT_HERE

    strcpy(plate + apple_sp * BUFSIZE, "apple");
    apple_sp++;

    GOT_HERE

    *apple_sp_ptr = apple_sp;
    commit_shared_ptr(apple_sp_ptr);

    GOT_HERE

    commit_shared_ptr(plate);

    GOT_HERE

    V(SEM_APPLE_SP);
    V(SEM_CAN_GET_APPLE);
}

int main(void) {
    int i;
    for (i = 0; i < TIMES; i++) {
        father();
    }
}

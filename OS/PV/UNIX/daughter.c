/******************************************
    daughter.c
    Copyright (c) 2005 Agent Zhang
    2005-12-29 2005-12-29
 ******************************************/ 

#include "app.h"
#include <string.h>

/*
    for (1..TIMES) {
        P('can_get_apple');
        P('apple_sp');
        my $fruit = $plate[--$apple_sp];
        print "  Daughter got an $fruit!\n";
        V('apple_sp');
        V('can_put');
    }
*/

void daughter(void) {
    int* apple_sp_ptr;
    int apple_sp;

    char* plate;
    char fruit[BUFSIZE];

    GOT_HERE

    P(SEM_CAN_GET_APPLE);
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

    --apple_sp;
    strncpy(fruit, plate + apple_sp * BUFSIZE, BUFSIZE);

    printf("  Daughter got an %s!\n", fruit);
    fflush(stdout);

    *apple_sp_ptr = apple_sp;
    commit_shared_ptr(apple_sp_ptr);

    GOT_HERE

    commit_shared_ptr(plate);

    GOT_HERE

    V(SEM_APPLE_SP);
    V(SEM_CAN_PUT);
}

int main(void) {
    int i;
    for (i = 0; i < TIMES; i++) {
        daughter();
    }
}

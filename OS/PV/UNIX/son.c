/******************************************
    son.c
    Copyright (c) 2005 Agent Zhang
    2005-12-29 2005-12-30
 ******************************************/

#include "app.h"
#include <string.h>

/*
    for (1..TIMES) {
        P('can_get_orange');
        P('orange_sp');
        my $fruit = $plate[++$orange_sp];
        print "  Son got an $fruit!\n";
        V('orange_sp');
        V('can_put');
    }
*/

void son(void) {
    int* orange_sp_ptr;
    int orange_sp;

    char* plate;
    char fruit[BUFSIZE];

    HERE

    P(SEM_CAN_GET_ORANGE);
    P(SEM_ORANGE_SP);

    HERE

    orange_sp_ptr = (int*) get_shared_mem(SHM_ORANGE_SP, SHM_ORANGE_SP_SZ);
    if (orange_sp_ptr == NULL) err("Can't get 'orange_sp'.");
    orange_sp = *orange_sp_ptr;

    fprintf(stderr, "orange_sp = %d\n", orange_sp);

    HERE

    plate = (char*) get_shared_mem(SHM_PLATE, SHM_PLATE_SZ);
    if (plate == NULL) err("Can't get 'plate'.");

    HERE

    ++orange_sp;
    strncpy(fruit, plate + orange_sp * BUFSIZE, BUFSIZE);

    printf("  Son got an %s!\n", fruit);
    fflush(stdout);

    *orange_sp_ptr = orange_sp;
    commit_shared_ptr(orange_sp_ptr);

    HERE

    commit_shared_ptr(plate);

    HERE

    V(SEM_ORANGE_SP);
    V(SEM_CAN_PUT);
}

int main(void) {
    int i;
    for (i = 0; i < TIMES; i++) {
        son();
    }
}

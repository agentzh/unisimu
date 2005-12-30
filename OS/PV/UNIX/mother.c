/******************************************
    mother.c
    Copyright (c) 2005 Agent Zhang
    2005-12-29 2005-12-29
 ******************************************/ 

#include "app.h"
#include <string.h>

/*
    for (1..TIMES) {
        P('can_put');
        P('orange_sp');
        print "Mother is putting an orange...\n";
        $plate[$orange_sp] = 'orange';
        $orange_sp--;
        V('orange_sp');
        V('can_get_orange');
    }
*/

void mother(void) {
    int* orange_sp_ptr;
    int orange_sp;

    char* plate;

    GOT_HERE

    P(SEM_CAN_PUT);
    P(SEM_ORANGE_SP);

    GOT_HERE

    orange_sp_ptr = (int*) get_shared_mem(SHM_ORANGE_SP, SHM_ORANGE_SP_SZ);
    if (orange_sp_ptr == NULL) err("Can't get 'orange_sp'.");
    orange_sp = *orange_sp_ptr;

    fprintf(stderr, "orange_sp = %d\n", orange_sp);

    GOT_HERE

    plate = (char*) get_shared_mem(SHM_PLATE, SHM_PLATE_SZ);
    if (plate == NULL) err("Can't get 'plate'.");

    printf("Mother is putting an orange...\n");
    fflush(stdout);

    strcpy(plate + orange_sp * BUFSIZE, "orange");
    orange_sp--;

    GOT_HERE

    *orange_sp_ptr = orange_sp;
    commit_shared_ptr(orange_sp_ptr);

    GOT_HERE

    commit_shared_ptr(plate);

    GOT_HERE

    V(SEM_ORANGE_SP);
    V(SEM_CAN_GET_ORANGE);
}

int main(void) {
    int i;
    for (i = 0; i < TIMES; i++) {
        mother();
    }
}

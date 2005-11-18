//: test.cpp
//: Copyright (c) Agent Zhang
//: 2004-12-05 2005-11-18

#include <stdio.h>

#define MIN_INTERVAL 1
#include "david.h"

#define OUTPUT(s) fprintf( stderr, "LINE %d : %s\n", __LINE__, (s));

void main() {
    OUTPUT( "Starting..." );

    BREAK();

    OUTPUT( "Good morning!" );

    BREAK();

    OUTPUT( "Good afternoon!" );

    BREAK();

    OUTPUT( "Good evening!" );
    
    BREAK();

    OUTPUT( "Night!" );

    BREAK();
}

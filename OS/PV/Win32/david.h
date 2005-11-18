//: david.h
//: Agent2002. All rights reserved.
//: 04-12-05 04-12-05

#ifndef DAVID_H_
#define DAVID_H_

#include <stdio.h>
#include <time.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#ifndef MIN_INTERVAL
#define MIN_INTERVAL 1 // in seconds
#endif

BOOL BREAK() 
{ 
    static HANDLE hMutex;
    static HANDLE hMutex2;
    static BOOL first = TRUE;
    DWORD dwWaitResult;
    static clock_t lastTime;
    long elapsed;
    
    hMutex = OpenMutex( 
            SYNCHRONIZE,
            FALSE,
            "MutexToDavidDebugger");  // name of mutex

    if (hMutex == NULL) 
    {
        fprintf( stderr, "Can't open the mutex object!\n" );
        return FALSE;
    }

    // Create a mutex with no initial owner.
    if (first) {
        hMutex2 = CreateMutex( 
            NULL,                       // no security attributes
            FALSE,                      // initially not owned
            "Mutex2ToDavidDebugger");  // name of mutex
        first = FALSE;
    } else {
        elapsed = (clock() - lastTime) / CLOCKS_PER_SEC;
        if (elapsed < MIN_INTERVAL) {
            Sleep( (MIN_INTERVAL - elapsed) * 1e3 );
        }

        if (! ReleaseMutex(hMutex)) { 
            // Deal with error.
            fprintf( stderr, "Can't release the Mutex object!\n" );
        } 
    }

    // Request ownership of mutex.
 
    dwWaitResult = WaitForSingleObject( 
        hMutex,   // handle to mutex
        INFINITE);   // five-second time-out interval
 
    switch (dwWaitResult) 
    {
        // The thread got mutex ownership.
        case WAIT_OBJECT_0:
            lastTime = clock();
            return TRUE;

        // Cannot get mutex ownership due to time-out.
        case WAIT_TIMEOUT:
            fprintf( stderr, "Waiting is timeout!\n" );
            return FALSE; 

        // Got ownership of the abandoned mutex object.
        case WAIT_ABANDONED:
            fprintf( stderr, "Wait for an abandoned Mutex object!\n" );
            return FALSE; 
    }
    
    return TRUE; 
}

#endif  // DAVID_H_

#include "pv.h"
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <errno.h>
#include <string.h>

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

int P(int key) {
    int semid;
    struct sembuf p_buf;

    semid = semget(key, 1, 0777);
    p_buf.sem_num = 0;
    p_buf.sem_op = -1;
    p_buf.sem_flg = 0;

    if (semop(semid, &p_buf, 1) == -1) {
        fprintf(stderr, "P(semid) failed\n");
        return FALSE;
    }
    return TRUE;
}

int V(int key) {
    int semid;
    struct sembuf v_buf;

    semid = semget(key, 1, 0777);
    v_buf.sem_num = 0;
    v_buf.sem_op = 1;
    v_buf.sem_flg = 0;
    if (semop(semid, &v_buf, 1) == -1) {
      fprintf(stderr, "V(semid) failed\n");
      return FALSE;
    }
    return TRUE;
}

static void set_sembuf_struct(struct sembuf *sem,int semnum, int semop,int semflg) {
    sem->sem_num=semnum;
    sem->sem_op=semop;
    sem->sem_flg=semflg;
}

int create_shared_mem(int key, int len) {
    int shmid;
    char *addr;
    shmid = shmget(key, len, 0777|IPC_CREAT|IPC_EXCL);
    if (shmid  == -1) {
        if (errno == EEXIST) {
           	shmid = shmget(key, len, 0777);
          	if (shmctl(shmid, IPC_RMID, 0) >= 0)
                shmid = shmget(key, len, 0777|IPC_CREAT|IPC_EXCL);
            else
                return FALSE;
        } else {
            return FALSE;
        }
    }
    addr = (char*)shmat(shmid, 0, 0);
    memset(addr, 0, len);
    shmdt(addr);
    return TRUE;
}

int create_sema(int key, int val) {
    int semid;
    struct sembuf sem_tmp;
    semid = semget(key, 1, 0777|IPC_CREAT|IPC_EXCL);
    if(semid == -1) {
        if (errno == EEXIST) {
            semid = semget(key, 1, 0777);
            if (semctl(semid, 0, IPC_RMID) >= 0)
                semid = semget(key, 1, 0777|IPC_CREAT|IPC_EXCL);
            else
                return FALSE;
        } else {
            return FALSE;
        }
    }
    set_sembuf_struct(&sem_tmp, 0, val, 0);
    semop(semid, &sem_tmp, 1);
    return TRUE;
}

void* get_shared_mem(int key, int len) {
    int shmid;
    shmid = shmget(key, len, 0777);
    return shmat(shmid, NULL, 0);
}

void commit_shared_ptr(void* ptr) {
    shmdt(ptr);
}

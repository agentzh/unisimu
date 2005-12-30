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

/* the mystic number 0777 stands for (S_IRWXU | S_IRWXG | S_IRWXO)
   see UNIX reference for details.
*/

int P(int key) {
    int semid;
    struct sembuf p_buf;

    semid = semget(key, 1, 0777);
    p_buf.sem_num = 0;
    p_buf.sem_op = -1;
    p_buf.sem_flg = 0;

    if (semop(semid, &p_buf, 1) == -1) {
        perror("P operation failed: ");
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
      perror("V operation failed: ");
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
    struct shmid_ds shm_desc;

    shmid = shmget(key, len, 0777|IPC_CREAT|IPC_EXCL);
    if (shmid  == -1) {
        if (errno == EEXIST) {
           	shmid = shmget(key, len, 0777);
          	if (shmctl(shmid, IPC_RMID, &shm_desc) >= 0) {
                shmid = shmget(key, len, 0777|IPC_CREAT|IPC_EXCL);
            } else {
                perror("create_shared_mem: shared memory exists and can't be removed: ");
                return FALSE;
            }
        } else {
            perror("create_shared_mem: ");
            return FALSE;
        }
    }
    addr = (char*)shmat(shmid, NULL, 0);
    memset(addr, 0, len);
    shmdt(addr);
    return TRUE;
}

int create_sema(int key, int val) {
    int semid;
    struct sembuf sem_tmp;
    semid = semget(key, 1, 0777|IPC_CREAT|IPC_EXCL);
    if(semid == -1) {
        perror("create_sema: ");
        if (errno == EEXIST) {
            semid = semget(key, 1, 0777);
            if (semctl(semid, 0, IPC_RMID) >= 0) {
                semid = semget(key, 1, 0777|IPC_CREAT|IPC_EXCL);
                if (semid == -1) {
                    perror("create_sema: semget: ");
                    return FALSE;
                }
            } else {
                perror("create_sema: semaphore exists and can't be removed: ");
                return FALSE;
            }
        } else {
            perror("create_sema: ");
            return FALSE;
        }
    }
    set_sembuf_struct(&sem_tmp, 0, val, 0);
    semop(semid, &sem_tmp, 1);
    return TRUE;
}

void* get_shared_mem(int key, int len) {
    int shmid;
    void* shm_addr;
    shmid = shmget(key, len, 0777);
    shm_addr = shmat(shmid, NULL, 0);
    if (!shm_addr) { /* operation failed. */
        perror("get_shared_mem: shmat: ");
        return NULL;
    }
    return shm_addr;
}

int commit_shared_ptr(void* shm_addr) {
    if (shmdt(shm_addr) == -1) {
        perror("commit_shared_ptr: shmdt: ");
        return FALSE;
    }
    return TRUE;
}

/* de-allocate the shared memory segment. */
int free_shared_mem(int key, int len) {
    struct shmid_ds shm_desc;
    int shmid;

    shmid = shmget(key, len, 0777);
    if (shmid == -1 || shmctl(shmid, IPC_RMID, &shm_desc) == -1) {
        perror("free_shared_mem: ");
        return FALSE;
    }
    return TRUE;
}

int free_sema(int key) {
    int semid;

    semid = semget(key, 1, 0777);
    if (semid == -1 || semctl(semid, 0, IPC_RMID) == -1) {
        perror("free_sema: ");
        return FALSE;
    }
}

#ifndef _PV_H_
#define _PV_H_

int P(int key);

int V(int key);

int create_shared_mem(int key, int len);

int create_sema(int key, int val);

void* get_shared_mem(int key, int len);

void commit_shared_ptr(void* ptr);

#endif

#ifndef _PV_H_
#define _PV_H_

/* 下面返回类型为 int 的函数都以返回 1 表示操作成功，
   返回 0 表示操作失败 */

/* P 操作，key 是信号量的键值 */
int P(int key);

/* V 操作，key 是信号量的键值 */
int V(int key);

/* 创建共享内存，key 是该内存对应的键值,
   len 是该内存的大小（以字节为单位）*/
int create_shared_mem(int key, int len);

/* 以指定的键值 key 创建信号量，并赋初值为 val */
int create_sema(int key, int val);

/* 获取键值 key 对应的共享内存的首地址,
   len 必须与实际大小完全一致 */
void* get_shared_mem(int key, int len);

/* 当完成对 get_shard_mem 返回的共享指针
   的操作以后，退回该指针给 OS */
void commit_shared_ptr(void* ptr);

#endif

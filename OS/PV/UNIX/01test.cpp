#include "cpptest.h"
#include "pv.h"

int main() {
    int key = 123456;
    
    test_plan(40);

    ok( create_sema(key, 2) );
    ok( P(key) );
    ok( P(key) );
    ok( V(key) );
    ok( P(key) );
    ok( V(key) );
    ok( V(key) );
    ok( P(key) );
    ok( P(key) );

    ok( create_sema(key, 2) );
    ok( P(key) );
    ok( P(key) );
    ok( V(key) );
    ok( P(key) );
    ok( V(key) );
    ok( V(key) );
    ok( P(key) );
    ok( P(key) );

    key = 563412;
    ok( create_shared_mem(key, 1) );
    char* c = (char*) get_shared_mem(key, 1);
    ok(c);
    is(*c, 0);
    *c = 21;
    commit_shared_ptr(c);

    c = (char*) get_shared_mem(key, 1);
    ok(c);
    is(*c, 21);
    commit_shared_ptr(c);

    int key2 = 563413;
    ok( create_shared_mem(key2, 3) );
    c = (char*) get_shared_mem(key2, 3);
    ok(c);
    is (c[0], 0);
    is (c[1], 0);
    is (c[2], 0);
    c[0] = 'a';
    c[1] = 'b';
    c[2] = '\0';
    commit_shared_ptr(c);

    c = (char*) get_shared_mem(key, 1);
    ok(c);
    is(*c, 21);
    commit_shared_ptr(c);

    c = (char*) get_shared_mem(key2, 3);
    ok(c);
    is(c, "ab");
    commit_shared_ptr(c);

    ok( create_shared_mem(key, 1) );
    c = (char*) get_shared_mem(key, 1);
    ok(c);
    is(*c, 0);
    commit_shared_ptr(c);

    ok( create_shared_mem(key2, 3) );
    c = (char*) get_shared_mem(key2, 3);
    ok(c);
    is (c[0], 0);
    is (c[1], 0);
    is (c[2], 0);
    commit_shared_ptr(c);

	summary();
    return 0;
}

#include <list>
#include <string.h>

using namespace std;

typedef struct PCB {
    int pid;
    int req_tm;
    int prio;
    char status;
} PCB;

typedef list<PCB> PCBList;

PCBList pcb_list;
PCBList end_list;

int prio_cmp(PCB a, PCB b) {
    if (a.prio < b.prio)
        return 0;
    else
        return 1;
}

// print out all the elements in the PCBList:
void dump_list(PCBList list) {
    printf("Process ID Request Time Priority Status\n");
    printf("---------- ------------ -------- -----\n");
    while (list.size()) {
        PCB pcb = list.front();
        list.pop_front();
        printf("%-10d %-12d %-11d %-7c\n",
            pcb.pid, pcb.req_tm, pcb.prio, pcb.status);
    }
}

// Print out the two lists to stdout:
void dump_all(void) {
     printf("------------ PCB List ----------------\n");
     dump_list(pcb_list);
     printf("\n------------ End List ----------------\n");
     dump_list(end_list);
     printf("\n");
}

int main(int argc, char* argv[]) {
    PCB pcb;
    int debug = 0;
	int batch = 0;

    for (int i = 0; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0)
            debug = 1;
		else if (strcmp(argv[i], "-b") == 0)
			batch = 1;
    }

    // Initialize the PCB list:
    for (int i = 1; i <= 5; i++) {
        pcb.pid = i;
        pcb.req_tm = 3+i;
        pcb.prio = 15-i;
        pcb.status = 'R';

        pcb_list.push_back(pcb);
    }

    // Perform the simulation process:
    while (pcb_list.size()) {
        pcb = pcb_list.front();
        pcb_list.pop_front();
        if (pcb.req_tm > 0) {
            printf("Running P%d...\n", pcb.pid);
            pcb.prio--; pcb.req_tm--;
            if (pcb.req_tm > 0) {
                pcb_list.push_front(pcb);
                pcb_list.sort(prio_cmp);
                if (debug) dump_all();
				if (!batch) getchar();
                continue;
            }
        }
        printf("Terminating P%d...\n", pcb.pid);
        pcb.status = 'E';
        end_list.push_back(pcb);
        if (debug) dump_all();
		if (!batch) getchar();
    }
    return 0;
}

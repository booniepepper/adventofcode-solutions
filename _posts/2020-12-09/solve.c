// gcc ./solve.c -o solve && ./solve

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *in;
    in = fopen("./input", "r");

    unsigned int *n = calloc(1, sizeof(unsigned int));
    unsigned int ns[25];

    #define read_n() fscanf(in, "%u\n", n)

    for (int i = 0; i < 25; i++) {
        read_n();
        ns[i] = *n;
    }

    int i = 0;
    while(read_n() > 0) {
        bool done = false;
        for (int j = 0; j < 25 && !done; j++) {
            for (int k = 0; k < 25 && !done; k++) {
                if (j !=k && ns[j] + ns[k] == *n) {
                    ns[i] = *n;
                    i = (i + 1) % 25;
                    done = true;
                }
            }
        }

        if (!done) {
            break;
        }
    }
    
    printf("Found a naughty number: %u!\n", *n);

    #undef read_n

    return 0;
}


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

    int ptr = 0;
    while(read_n() > 0) {
        bool done = false;
        for (int j = 0; j < 25 && !done; j++) {
            for (int k = 0; k < 25 && !done; k++) {
                if (j !=k && ns[j] + ns[k] == *n) {
                    ns[ptr] = *n;
                    ptr = (ptr + 1) % 25;
                    done = true;
                }
            }
        }

        if (!done) {
            break;
        }
    }
    
    // Part 1 solution
    int target = *n;
    printf("Found a naughty number: %u!\n", target);

    rewind(in);

    // TODO: This could be a lot cleaner, but got sleepy.
    char line[255];
    for (int i = 0; true; i++) {
        printf("Ok, from the top now...\n");
        rewind(in);

        for (int skip = 0; skip < i; skip++) {
            fgets(line, 255, in);
        }
        
        read_n();
        int start = *n, min = *n, max = *n;
        printf("%u, ", start);
        
        int sum = 0;
        bool done = false;

        while(!done && read_n()) {
            if (*n < min) { min = *n; }
            if (*n > max) { max = *n; }
            printf("%u, ", *n);

            sum += *n;
            if (sum == target) {
                // Part 2 solution
                printf("\nFound it!!!\nSum: %u\nStarted at %u and ended at %u\n", sum, start, *n);
                printf("Min: %u, Max: %u, Encryption weakness: %u", min, max, min + max);
                done = true;
            }
            else if (sum > target) {
                printf("\nTrying again... (sum: %u, target: %u)\n=========\n", sum, target);
                break;
            }
        }

        if (done) {
            break;
        }
    }

    #undef read_n

    return 0;
}


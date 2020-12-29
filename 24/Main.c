#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* #define LENGTH 9 */
/* #define STEPS 100 */
/* #define LENGTH 1000000 */
#define STEPS 10000000
#define STEPS 100000
#define min(x, y) (x < y ? x : y)
#define max(x, y) (x > y ? x : y)
#define intcpy(dst, src, sz) (memcpy(dst, src, max(0, sz) * sizeof(int)))

// update: just read a line of the first post on the solution thread on reddit
// and it makes a lot of sense: for each element, keep a pointer to the
// previous element. This whole time I was thinking that maybe I missed some
// smart way to shortcut the algorithm but this makes more sense.
// (I'm not sure right now how you would properly update the pointer but I
// guess it shouldn't be that hard)
// And I guess some sort of linked list might make more sense than an array
// I think the only reason I'm using an array is because it's the simplest data
// structure to use in C, and I'm using C because it's the easiest language to
// write fast code in for simple problems

int* cups;
int dropped[3];

int findIndex(int target) {
    for (int i = 0; i < LENGTH; i++) {
        if (cups[i] == target) return i;
    }
}

int targetIndex(int cur) {
    int target = ((cur - 2 + LENGTH) % LENGTH) + 1;
    return (dropped[0] == target || dropped[1] == target || dropped[2] == target) ?
        targetIndex(target) : findIndex(target);
}

int main() {
    /* int initial[] = {3,8,9,1,2,5,4,6,7}; // Test input */
    int initial[] = {1,3,7,8,2,6,4,9,5};
    cups = calloc(LENGTH, sizeof(int)); // allocate a bit extra to simplify copying
    int inputLength = sizeof(initial)/sizeof(initial[0]);
    intcpy(cups, initial, inputLength);
    for (int i = inputLength; i < LENGTH; i++) {
        cups[i] = i + 1;
    }
    // Before the crab starts, it will designate the first cup in your list as
    // the current cup.
    int current = 0;
    int target;
    for (int step = 0; step < STEPS; step++) {
        if (step % 1000 == 0) printf("step: %d\n", step);
        // The crab picks up the three cups that are immediately clockwise of
        // the current cup.
        for (int i = 0; i < 3; i++) {
            dropped[i] = cups[(current + i + 1 + LENGTH) % LENGTH];
        }
        // immediately clockwise of the destination cup.
        // The crab selects a destination cup.
        target = targetIndex(cups[current]);
        // They are removed from the circle; cup spacing is adjusted as
        // necessary to maintain the circle.
        if (current < target) {
            intcpy(cups + current + 1, cups + current + 4, target - current - 3);
        } else {
            intcpy(cups + current + 1, cups + current + 4, LENGTH - current - 4);
            int fromEnd = min(LENGTH - current - 1, 3);
            int fromBeginning = max(0, current - (LENGTH - 4));
            intcpy(cups + LENGTH - fromEnd, cups + fromBeginning, fromEnd);
            intcpy(cups, cups + 3, target - 2);
        }
        // The crab places the cups it just picked up so that they are
        // immediately clockwise of the destination cup.
        for (int i = 0; i < 3; i++) {
            cups[(target + i - 2 + LENGTH) % LENGTH] = dropped[i];
        }
        // The crab selects a new current cup: the cup which is immediately
        // clockwise of the current cup.
        current = (current + 1 + LENGTH) % LENGTH;
    }
    printf("\n\nend: ");
    for (int i = 0; i < LENGTH; i++) {
        printf("%d,", cups[i]);
    }
    printf("\n");
    for (int i = 0; i < 3; i++) {
        printf("%d,", dropped[i]);
    }
    free(cups);
}

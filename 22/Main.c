#include <stdio.h>
#include <stdlib.h>

#define MAX_STEP 30000000

int main() {
    int initial[] = {0,3,1,6,7,5};
    int* latest = calloc(MAX_STEP, sizeof(int));
    int turn = 0;
    int prev = 0;
    for (; turn < sizeof(initial)/sizeof(initial[0]); turn++) {
        latest[prev] = turn;
        prev = initial[turn];
    }
    for (; turn < MAX_STEP; turn++) {
        int pprev = latest[prev];
        latest[prev] = turn;
        prev = pprev ? turn - pprev : 0;
    }
    free(latest);
    printf("%d", prev);
}

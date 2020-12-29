#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* #define LENGTH 11 */
/* #define STEPS 10 */
/* #define LENGTH 9 */
/* #define STEPS 10 */
#define LENGTH 1000000
#define STEPS 10000000
#define min(x, y) (x < y ? x : y)
#define max(x, y) (x > y ? x : y)
#define intcpy(dst, src, sz) (memcpy(dst, src, max(0, sz) * sizeof(int)))

typedef struct node {
    int value;
    struct node* next;
} node;

node *previous;

int main() {
    /* int initial[] = {3,8,9,1,2,5,4,6,7}; // Test input */
    int initial[] = {1,3,7,8,2,6,4,9,5};
    node* current = malloc(sizeof(node));
    node* loopNode = current;
    node** prevs = malloc(sizeof(node)*(LENGTH + 1));
    int inputLength = sizeof(initial)/sizeof(initial[0]);
    for (int i = 0; i < inputLength; i++) {
        prevs[initial[i] + 1] = loopNode;
        loopNode->value = initial[i];
        loopNode->next = malloc(sizeof(node));
        loopNode = loopNode->next;
    }
    for (int i = inputLength; i < LENGTH - 1; i++) {
        prevs[i + 2] = loopNode;
        loopNode->value = i + 1;
        loopNode->next = malloc(sizeof(node));
        loopNode = loopNode->next;
    }
    prevs[1] = loopNode;
    loopNode->value = LENGTH;
    loopNode->next = current;
    for (int i = 0; i < STEPS; i++) {
        node* dropped = current->next;
        current->next = current->next->next->next->next;
        node* prev = prevs[current->value];
        while (prev == dropped || prev == dropped->next || prev == dropped->next->next) {
            prev = prevs[prev->value];
        }
        dropped->next->next->next = prev->next;
        prev->next = dropped;
        current = current->next;
    }
    loopNode = current;
    while (loopNode->value != 1) {
        loopNode = loopNode->next;
    }
    printf("%ld", ((long)loopNode->next->value) *
            ((long)loopNode->next->next->value));
    // can't be bothered to free the nodes
}

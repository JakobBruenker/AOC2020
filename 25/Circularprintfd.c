#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LENGTH 9
#define STEPS 10
/* #define LENGTH 1000000 */
/* #define STEPS 10000000 */
#define min(x, y) (x < y ? x : y)
#define max(x, y) (x > y ? x : y)
#define intcpy(dst, src, sz) (memcpy(dst, src, max(0, sz) * sizeof(int)))

typedef struct node {
    int value;
    struct node* next;
} node;

node *previous;

int main() {
    int initial[] = {3,8,9,1,2,5,4,6,7}; // Test input
    /* int initial[] = {1,3,7,8,2,6,4,9,5}; */
    node* current = malloc(sizeof(node));
    node* loopNode = current;
    node** prevs = malloc(sizeof(node)*(LENGTH + 1));
    int inputLength = sizeof(initial)/sizeof(initial[0]);
    for (int i = 0; i < inputLength - 1; i++) {
        if (initial[i] == LENGTH) {
            prevs[1] = loopNode;
        } else {
            prevs[initial[i] + 1] = loopNode;
        }
        loopNode->value = initial[i];
        loopNode->next = malloc(sizeof(node));
        loopNode = loopNode->next;
    }
    // TODO loop for vals ouside of initial
    prevs[initial[inputLength - 1] + 1] = loopNode;
    {for (int i = 1; i < LENGTH + 1; i++) {
        printf("%d|", prevs[i]->value);
    }
    printf("\n");}
    loopNode->value = initial[inputLength - 1];
    loopNode->next = current;
    for (int i = 0; i < STEPS; i++) {
    {node* loopNode_ = current;
    for (int i = 0; i < LENGTH + 1; i++) {
        printf("%d,", loopNode_->value);
        loopNode_ = loopNode_->next;
    }
    printf("\n");}
        node* dropped = current->next;
    {node* loopNode_ = dropped;
    for (int i = 0; i < 3; i++) {
        printf("%d,", loopNode_->value);
        loopNode_ = loopNode_->next;
    }
    printf("\n");}
        current->next = current->next->next->next->next;
        printf("finding prev for %d:\n", current->value);
        node* prev = prevs[current->value];
        printf("preving %d...\n", prev->value);
        while (prev == dropped || prev == dropped->next || prev == dropped->next->next) {
            prev = prevs[prev->value];
        }
        printf("prev: %d\n", prev->value);
        dropped->next->next->next = prev->next;
        prev->next = dropped;
        current = current->next;
    }
    {node* loopNode_ = current;
    for (int i = 0; i < LENGTH * 3; i++) {
        printf("%d,", loopNode_->value);
        loopNode_ = loopNode_->next;
    }
    printf("\n");}
    printf("hi\n");
    // can't be bothered to free the nodes
}

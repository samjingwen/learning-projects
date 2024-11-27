#include <stdio.h>

void increment_ptr(int **h) {
    *h = *h + 1;
}

int main(void) {
    int A[3] = {50, 60, 70};
    int *q = A;
    increment_ptr(&q);
    printf("*q = %d\n", *q);
}


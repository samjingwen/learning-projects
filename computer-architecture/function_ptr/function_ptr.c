#include <stdio.h>

int x10(int), x2(int);

void mutate_map(int [], int n, int(*)(int));

void print_array(int [], int n);

int x2(int n) { return 2 * n; };

int x10(int n) { return 10 * n; };

void mutate_map(int A[], int n, int(*fp)(int)) {
    for (int i = 0; i < n; i++) {
        A[i] = (*fp)(A[i]);
    }
}

void print_array(int A[], int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", A[i]);
    }
    printf("\n");
}

int main(void) {
    int A[] = {3, 1, 4}, n = 3;
    print_array(A, n);

    mutate_map(A, n, &x2);
    print_array(A, n);

    mutate_map(A, n, &x10);
    print_array(A, n);

    return 0;
}














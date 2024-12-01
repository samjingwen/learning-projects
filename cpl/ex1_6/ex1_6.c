#include <stdio.h>

int main(void) {
    int c;

    c = getchar();
    while (c != EOF) {
        putchar(c);
        c = getchar();
    }

    printf("c == EOF evaluates to: %d\n", c == EOF);
    return 0;
}

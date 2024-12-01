#include <stdio.h>


int main(void) {
    int c = getchar();

    while (c != EOF) {
        if (c == '\t') {
            putchar('\\');
            putchar('t');
        } else {
            putchar(c);
        }

        c = getchar();
    }
}
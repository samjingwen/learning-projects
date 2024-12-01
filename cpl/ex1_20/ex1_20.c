#include <stdio.h>

#define TAB_STOP 8

int spaces_to_next_tab_stop(int column) {
    return TAB_STOP - (column % TAB_STOP);
}

int main(void) {
    int ch, column = 0;
    while ((ch = getchar()) != EOF) {
        if (ch == '\t') {
            int spaces = spaces_to_next_tab_stop(column);
            for (int i = 0; i < spaces; ++i) {
                putchar(' ');
            }
            column = 0;
        } else if (ch == '\n') {
            putchar(ch);
            column = 0;
        } else {
            putchar(ch);
            ++column;
        }
    }
    return 0;
}
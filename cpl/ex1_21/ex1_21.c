#include <stdio.h>

#define TAB_STOP 8

int spaces_to_next_tab_stop(int column) {
    return TAB_STOP - (column % TAB_STOP);
}

int main(void) {
    int column = 0, spaces = 0;
    int ch;
    while ((ch = getchar()) != EOF) {
        if (ch == ' ') {
            ++spaces;
        } else {
            while (spaces > 0) {
                int padding = spaces_to_next_tab_stop(column);
                if (spaces < padding) {
                    --spaces;
                    ++column;
                } else {
                    spaces -= padding;
                    column += padding;
                    putchar('\t');
                }
            }
            putchar(ch);
            ++column;
        }
    }
    return 0;
}


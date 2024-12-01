#include <stdio.h>

int main(void) {
    int nwhite = 0, nother = 0;
    int nletter[26];

    for (int i = 0; i < 26; ++i) {
        nletter[i] = 0;
    }

    int c;
    while ((c = getchar()) != EOF) {
        if (c >= 'a' && c < 'z') {
            ++nletter[c-'a'];
        } else if (c == ' ' || c == '\n' || c == '\t') {
            ++nwhite;
        } else {
            ++nother;
        }
    }

    printf("digits =");
    for (int i = 0; i < 26; ++i) {
        printf(" %d", nletter[i]);
    }
    printf(", white space = %d, other = %d\n", nwhite, nother);
}
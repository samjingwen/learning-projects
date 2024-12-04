#include <stdio.h>

int strindex(char s[], char t[]) {
    int result = -1;

    int i = 0;
    for (; s[i] != '\0'; i++) {
        int k = 0;
        for (int j = i; t[k] != '\0' && s[j] == t[k]; j++, k++);

        if (k > 0 && t[k] == '\0') {
            result = i;
        }
    }

    return result;
}

int main(void) {
    printf("%d\n", strindex("hello world world", "world"));
}
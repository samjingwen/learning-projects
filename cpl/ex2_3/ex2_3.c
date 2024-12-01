#include <ctype.h>
#include <stdio.h>

int hex_to_int[256] = {
        ['0'] = 0, ['1'] = 1, ['2'] = 2, ['3'] = 3, ['4'] = 4,
        ['5'] = 5, ['6'] = 6, ['7'] = 7, ['8'] = 8, ['9'] = 9,
        ['A'] = 10, ['B'] = 11, ['C'] = 12, ['D'] = 13, ['E'] = 14, ['F'] = 15,
        ['a'] = 10, ['b'] = 11, ['c'] = 12, ['d'] = 13, ['e'] = 14, ['f'] = 15
};

int htoi(const char s[]) {
    int i = 0;
    if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        i = 2;
    }

    int n = 0;
    for (; s[i] != '\0'; ++i) {
        n = 16 * n + hex_to_int[s[i]];
    }
    return n;
}

int main(void) {
    const char *hex1 = "0x1A3F";
    const char *hex2 = "1a3f";
    const char *hex3 = "FF";
    const char *hex4 = "0X4B";

    printf("Hexadecimal: %s, Decimal: %d\n", hex1, htoi(hex1));
    printf("Hexadecimal: %s, Decimal: %d\n", hex2, htoi(hex2));
    printf("Hexadecimal: %s, Decimal: %d\n", hex3, htoi(hex3));
    printf("Hexadecimal: %s, Decimal: %d\n", hex4, htoi(hex4));

    return 0;
}

#include <stdio.h>

int fahr_to_celsius(int fahr) {
    return 5 * (fahr - 32) / 9;
}

int main(void) {
    int lower = 0;
    int upper = 300;
    int step = 20;

    int fahr = lower;
    while (fahr <= upper) {
        int celsius = fahr_to_celsius(fahr);
        printf("%d\t%d\n", fahr, celsius);
        fahr = fahr + step;
    }
    return 0;
}
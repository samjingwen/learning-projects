#include <stdio.h>

int main(void) {
    printf("%-12s%s\n", "Fahrenheit", "Celsius");

    float lower = 0;
    float upper = 300;
    float step = 20;

    float fahr = lower;
    while (fahr <= upper) {
        float celsius = (5.0 / 9.0) * (fahr - 32.0);
        printf("%-12.0f%7.1f\n", fahr, celsius);
        fahr += step;
    }
    return 0;
}
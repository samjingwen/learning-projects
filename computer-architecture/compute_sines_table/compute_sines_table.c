#include <stdio.h>
#include <math.h>

int main(void) {
    printf("\nCompute a table of the sine function\n\n");
    double pi = 4.0*atan(1.0);
    printf("π= %f \n\n", pi);
    printf("Angle\tSine\n");
    int angle_degree = 0;
    while (angle_degree <= 360) {
        double angle_radian = pi * angle_degree / 180.0;
        double value = sin(angle_radian);
        printf("%3d\t%f\n", angle_degree, value);
        angle_degree += 10;
    }
    return 0;
}



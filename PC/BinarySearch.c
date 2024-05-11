#include <stdio.h>

/*
 *
  3.11. Să se scrie un program recursiv pentru căutarea eficientă a unei valori într-un
  tablou care conține numere reale ordonate crescător
 *
 */

int binary_search(double array[], double value, int start, int end) {
    // base case: value not found
    if (start > end) {
        return -1;
    }

    // find the midpoint of the current subarray
    int mid = (start + end) / 2;

    // if the value is at the midpoint, return its index
    if (array[mid] == value) {
        return mid;
    }

        // if the value is less than the midpoint, search the left half of the array
    else if (value < array[mid]) {
        return binary_search(array, value, start, mid - 1);
    }

        // if the value is greater than the midpoint, search the right half of the array
    else {
        return binary_search(array, value, mid + 1, end);
    }
}

int main(void) {
    double array[] = {1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.9, 31, 34};
    double value = 7.9;
    int index = binary_search(array, value, 0, sizeof (array)/sizeof (double));
    printf("%d\n", index);  // prints 2
    return 0;
}
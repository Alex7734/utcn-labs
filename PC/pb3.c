#include <stdio.h>
#include <stdlib.h>


int is_even(int x) {
    return x % 2 == 0;
}

int paritate(int *a, int n) {
    if (n == 1) {
        return 1;
    }

    if (is_even(list[0]) == is_even(a[1])) {
        return paritate(a + 1, n - 1);
    } else {
        return 0;
    }
}

int main (){
    int a[] = {58, -24, 2, 0, 4890};
    int n = 5;
    printf("%d", paritate(a, n));
    return 0 ;
}

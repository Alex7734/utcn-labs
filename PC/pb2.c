#include <stdio.h>
#include <stdlib.h>
#include <math.h>


int* divizori(int x, int* n){
    int* arr = malloc(sizeof(int) * (int)sqrt(x));
    for(int i=1; i<=(int)(sqrt(x)); i++){
        if (x % i == 0){
            arr[(*n)++] = i;
        }
    }
    arr[(*n)++] = x;
    return arr;
}

int main (){
    int nr;
    scanf("%d", &nr);

    for (int i=0; i<nr; i++){
        int x;
        int divNumber = 0;
        scanf("%d", &x);
        int* arr = divizori(x, &divNumber);
        for (int j=0; j<divNumber; j++){
            printf("%d ", arr[j]);
        }
        printf("\n");
        free(arr);
    }

    return 0 ;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 pb 5
int** citesteMat(FILE* f, int* rows, int* cols, int* x, int* y){
    fscanf(f, "%d %d", rows, cols);
    int **mat = malloc(sizeof(int *) * (*rows));
    for (int i=0; i<(*rows); i++){
        mat[i] = malloc(sizeof(int) * (*cols));
        for (int j = 0; j < (*cols); j++) {
            fscanf(f,"%d", &mat[i][j]);
        }
    }
    fscanf(f, "%d %d", x, y);
    fclose(f);
}

void judecaMat(int **mat, int x, int y) {
    // spre stanga
    int sumStanga = 0;
    int ys = y;
    while (ys > 0) {
        ys--;
        sumStanga += mat[x][ys];
    }
    // punct punct punct analog
}

int main() {
    int rows, cols, x, y;
    FILE *f = fopen("gradina.txt", "r");
    int **mat = citesteMat(f, &rows, &cols, &x, &y);
    judecaMat(mat, x, y);
}
*/
/*
pb 4

int distanta_minima(char s[], int n) {
    static int min = 100;
    if (n == 2) {
        if (min == 100) min = 0;
        return min;
    }

    int cmp = s[n-1] - s[n - 2];

    if (cmp < 0) {
        if (abs(cmp) < min)
            min = abs(cmp);
    }
    distanta_minima(s, n- 1);
}


int main() {
    char s[10] = {'a', 'c', 'd', 'i', 'e', 'c', 'a', 'y', 'y', 'b'};
    int n = 10;
    int x = distanta_minima(s,n);
    printf("%d\n", x);
    return 0;
}

*/
/*

PR 3

typedef struct {
    char numeFisier[20];
    char extensie[3];
    int nrBiti;
    float dim;
} imagine;

void populare (imagine* arr){
    float d = 500.0f;
    for (int i = 0; i < 5000; i++) {
        char *jpg = strdup("jpg");
        strcpy(arr[i].extensie, jpg);
        arr[i].dim = d;
        d -= 0.1f;
        if (i % 2 == 0){
            arr[i].nrBiti = 24;
        } else {
            arr[i].nrBiti = 8;
        }
        char *nume = malloc(20);
        sprintf(nume, "IMG_%04d", i);
        strcpy(arr[i].numeFisier, nume);
    }
};

int main() {
    imagine* arr = malloc(sizeof(imagine) * 5000);
    populare(arr);
    FILE *f = fopen("test.bin", "wb");
    fwrite(arr, sizeof(imagine), 5000, f);
    return 0;
}
 */


/*
 Pr2
int main() {
    int g1, g2, m1, m2, s1, s2;
    char x1, x2;
    if (scanf("%d*%d'%d\"%c\n%d*%d'%d\"%c", &g1, &m1, &s1, &x1, &g2, &m2, &s2, &x2) != 8) {
        printf("Invalid");
        return 1;
    }

    if (x1 != 'N' && x1 != 'S' && x2 != 'N' && x2 != 'S'){
        if (x1 != x2) {
            int carry = 0;
            int s = s1 + s2;
            if (s > 60) {
                s -= 60;
                carry = 1;
            }
            int m = m1 + m2 + carry;
            if (m < 60) carry = 0;
            if (m > 60) {
                m -= 60;
                carry = 1;
            }
            int g = (g1 + g2 + carry) % 90;


        }
    }

    return 0;
}

*/

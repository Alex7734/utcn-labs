//
// Created by Lenovo on 12/23/2022.
//
#include <stdio.h>
#include <stdlib.h>
/*
 *
   3.2. Folosind numai pointeri şi expresii cu pointeri să se scrie funcţii de citire, afişare
   şi înmulţire a două matrice.
 *
 */


// Alocare dinamica de matrice
int **matrix_new(int rows, int cols) {
    int **ret;
    // Important!!! Calloc --> Deoarece avem nevoie de o matrice intializata cu 0 pentru result
    ret = calloc(rows,sizeof(int *));
    for (int i = 0; i < rows; ++i) {
        ret[i] = calloc(cols,sizeof(int));
    }
    return ret;
}

// Citire matrice
void citireMat(int rows, int cols, int** matrix){
    for(int i=0; i<rows; i++){
        for (int j=0; j<cols; j++){
            printf("element[%d,%d]=", i+1,j+1);
            scanf("%d", &matrix[i][j]);
        }
    }
}

// Afisare matrice
void afisareMat(int rows, int cols, int** matrix){
    for(int i=0; i<rows; i++){
        for (int j=0; j<cols; j++){
            printf("%d ", matrix[i][j]);
        }
        printf("\n");
    }
}

// Prelucrarea unei matrice
int** inmultirea (int size, int** A, int** B, int** result){
    for (int i=0; i<size; i++){
        for(int j=0; j<size; j++){
            for(int k=0; k<size; k++){
                result[i][j] = result[i][j] + (A[i][k]*B[k][j]);
            }
        }
    }
    return result;
}

int lab8() {
    int n;
    printf("Matrie de NxN, n=");
    scanf("%d", &n);
    int **matrix1 = matrix_new(n, n);
    citireMat(n,n,matrix1);
    int **matrix2 = matrix_new(n, n);
    citireMat(n,n,matrix2);

    int **result = matrix_new(n, n);
    inmultirea(n,matrix1,matrix2, result);

    afisareMat(n,n,result);

    return 0;
}

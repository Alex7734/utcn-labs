#include <stdio.h>
#include <stdlib.h>
/*
 *
3.9. Să se scrie o funcție pentru calculul matricei A^k, unde A este o matrice pătratică.
Se vor utiliza pointeri și expresii cu pointeri pentru accesul la elementele matricei. Matricea
inițială și cea rezultată vor fi ulterior afișate în format natural (sub formă de linii și coloane).
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
int** inmultirea (int size, int** A, int putere, int** result){
    for (int p=0; p<putere; p++){
        for (int i=0; i<size; i++){
            for(int j=0; j<size; j++){
                for(int k=0; k<size; k++){
                    result[i][j] = result[i][j] + (A[i][k]*A[k][j]);
                }
            }
        }
    }
    return result;
}

int main() {
    int n;
    int k;
    printf("Matrie de NxN, n=");
    scanf("%d", &n);
    int **matrix1 = matrix_new(n, n);
    citireMat(n,n,matrix1);
    afisareMat(n,n, matrix1);
    printf("\n La ce putere o punem:");
    scanf("%d",&k);

    printf("Matricea la puterea %d este\n", k);
    int **result = matrix_new(n, n);
    inmultirea(n,matrix1, k, result);

    afisareMat(n,n,result);

    return 0;
}

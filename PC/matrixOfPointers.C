#include <stdio.h>
#include <stdlib.h>
#include <string.h>
// Project headers
#include <exception.h>
#include <matrixGeneration4.h>
#include <memoryLeaks.h>

typedef char *string_t;

char ***matrixGeneration4(unsigned rows, unsigned cols) {
    if (rows < 1 || rows > 10 || cols < 1 || cols > 10){
        setErrorInfo(INVALID_RANGE);
        return NULL;
    }
    setErrorInfo(OK);

    int elemTrack = 1;

    string_t **matrix = calloc(rows, sizeof (string_t));
    for (int i=0; i<rows; i++){
        matrix[i] = calloc(cols, sizeof (string_t));
        for (int j=0; j<cols; j++){
            // Used to get the length of a string
            // Very useful in pair with sprintf
            int len = snprintf(NULL, 0, "elem_%d", elemTrack);
            matrix[i][j] = calloc(len+1, sizeof(char));
            // after alocating enough space for the string with +1 you can put it in there
            sprintf(matrix[i][j], "elem_%d", elemTrack++);
        }
    }

    return matrix;
}

void matrixGeneration4_free(char ***matrix, unsigned rows, unsigned cols) {

    if (matrix == NULL){
        setErrorInfo(NULL_POINTER);
        return;
    }
    setErrorInfo(OK);

    for (int i=0; i<rows; i++){
        for (int j=0; j<cols; j++){
            free(matrix[i][j]);
        }
        free(matrix[i]);
    }
    free(matrix);

}

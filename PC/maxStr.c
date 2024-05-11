#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
Lab 9
 3.7. Să se scrie un program care citeşte n şiruri de caractere şi afişează şirul cel mai
 lung şi şirul cel mai mare alfanumeric.

 */

char* read_string() {
    int size = 8;// Size of the string
    char* str = malloc(size);  // Pointer to the dynamic string
    char c;             // Character read from stdin
    int i = 0;
    // Read characters from stdin until EOF or newline
    while ((c = (char)getchar()) != '\n') {
        // Increase size of string and reallocate memory
        if (i==size){
            str = realloc(str, size*2);
        }
        if (str == NULL) {
            // Allocation failed, exit program
            perror("realloc");
            exit(1);
        }

        // Add character to the string
        str[i++] = c;
    }

    // Add null character to the end of the string
    str[i] = '\0';
    return str;
}

char* maxInArrOfStrings(int size, char** array){
    int index = 0;
    for (int i=1; i<size; i++){
        if (strcmp(array[i],array[index])>0)
            index = i;
    }
    return array[index];
}

int main() {
    int n;
    printf("N:");
    scanf("%d ",&n);
    char** arr = malloc(sizeof (char*) * n);
    for (int i=0; i<n; i++){
        arr[i] = read_string();
    }

    char* max = NULL;
    max = maxInArrOfStrings(n, arr);
    printf("The maximum of the string array is %s", max);

    for(int i=0; i<n; i++){
        free(arr[i]);
    }
    free(arr);

    return 0;
}
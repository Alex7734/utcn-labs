#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define true 1
#define false 0

/*
  3.2. Se va scrie o funcţie care să realizeze extragerea dintr-un şir de caractere sursă a
  unui subşir specificat prin poziţia în cadrul sursei şi a numărului de caractere extrase.
 */

char* read_string() {
    int size = 8;// Size of the string
    char* str = malloc(size);  // Pointer to the dynamic string
    char c;             // Character read from stdin
    int i = 0;
    // Read characters from stdin until EOF or newline
    while ((c = getchar()) != '\n') {
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

// Copies a string from
char* copy(char* string, int t, int k){
    char* result = calloc(k + 1, sizeof(char));
    int toWhere = t+k;
    int i = 0;
    while (t<toWhere){
        result[i] = string[t];
        t++;
        i++;
    }
    result[k] = '\0';
    return result;
}

int main(void) {
    char* str = read_string();
    char* partOfString = copy(str, 1, 3);
    printf("\n%s\n", partOfString);
    free(str);
    return 0;
}
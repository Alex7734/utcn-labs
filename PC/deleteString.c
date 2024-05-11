#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
    3.4. Se va scrie o funcţie pentru ştergerea unui subşir dintr-un şir de caractere dat.
    Subşirul se va specifica prin poziţie şi număr de caractere.

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

char* delete_substring(char* str, int start, int length) {
    // Calculate the length of the string
    int str_length = (int)strlen(str);
    char* aux;
    aux = (char*)malloc(str_length);

    strcpy(aux, str);

    // Check if the start position and length are valid
    if (start < 0 || start >= str_length || length < 0 || start + length > str_length) {
        printf("Invalid start position or length\n");
        return 0;
    }

    // Shift the characters after the deleted substring to the left
    for (int i = start; i < str_length - length; i++) {
        aux[i] = aux[i + length];
    }

    // Add a null terminator to the end of the string
    aux[str_length - length] = '\0';
    return aux;
}

int main() {
    char* str = read_string();
    char* NewString = delete_substring(str, 1, 3);
    if(NewString != NULL)
        printf("%s\n", NewString);
    return 0;
}

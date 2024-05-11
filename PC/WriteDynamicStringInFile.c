#include <stdio.h>
#include <stdlib.h>


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

int main(void) {
    FILE* fp = fopen("test.txt", "w+");
    if (fp == NULL) {
        perror("fopen");
        exit(1);
    }

    char* str = read_string();
    fputs(str, fp);
    fclose(fp);

    free(str);
    return 0;
}
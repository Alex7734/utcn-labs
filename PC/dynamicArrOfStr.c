#include <stdio.h>
#include <stdlib.h>

/*
 *
 * Given a file return an array of the sizes of each line.
 * Given the number of lines in the files store each line in a dynamic arr
 *
 */


int main()
{
    int n = 4;
    FILE *file;
    file = fopen("test.txt", "r");

    if (file == NULL)
    {
        printf("Error opening file!\n");
        return 1;
    }

    int *sizes = malloc(n*sizeof (int));
    int total = 0;
    char ch;
    int i = 0;
    while (!feof(file) && !ferror(file))
    {
        fscanf(file, "%c", &ch);

        if (ch == '\n' || ch == EOF){
            sizes[i] = total;
            i++;
            total = 0;
            continue;
        }

        total++;
    }

    printf("Sizes arr:");

    for (int k=0; k<n; k++){
        printf("%d ", sizes[k]);
    }

    printf("\n");

    if (ferror(file))
    {
        printf("Error reading from file!\n");
        return 1;
    }


    char **arr = malloc(sizeof(char *)*n);
    rewind(file);
    int index = 0;

    while (!feof(file) && index<n)
    {
        arr[index] = malloc(sizeof(char)*sizes[index]);
        for (int j=0; j<sizes[index]; j++){
            fscanf(file, "%c", &arr[index][j]);
        }
        arr[index][sizes[index]] = '\0';
        index++;
    }


    if (ferror(file))
    {
        printf("Error reading from file!\n");
        return 1;
    }


    fclose(file);

    for (int p=0; p<n; p++){
        printf("%s", arr[p]);
    }

    for (i=0; i<n; i++){
        free(arr[i]);
    }
    free(arr);
    return 0;
}
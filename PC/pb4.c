#include <stdio.h>
#include <string.h>

#define MAX_WORD_LENGTH 20
#define MAX_WORDS 1000

int max_board_length;
char words[MAX_WORDS][MAX_WORD_LENGTH];
int word_count;

// Function to read the data from the input file
int read_data() {
    FILE *fp;
    fp = fopen("textul.txt", "r");
    if (fp == NULL) {
        printf("Error: Unable to open file\n");
        return 0;
    }

    fscanf(fp, "%d", &max_board_length);
    word_count = 0;
    while (fscanf(fp, "%s", words[word_count]) == 1) {
        word_count++;
    }

    fclose(fp);
    return 1;
}

// Function to find the pair of words that can be placed on the game board
int find_pair(char *first_word, char *second_word) {
    int i, j;
    for (i = 0; i < word_count; i++) {
        for (j = 0; j < word_count; j++) {
            if (i == j) continue;
            int length = strlen(words[i]) + strlen(words[j]) + 1;
            if (length <= max_board_length) {
                strcpy(first_word, words[i]);
                strcpy(second_word, words[j]);
                return 1;
            }
        }
    }
    return 0;
}

// Function to display the content of the game board
void display_board(char *first_word, char *second_word) {
    int i, j;
    int first_length = strlen(first_word);
    int second_length = strlen(second_word);
    for (i = 0; i < first_length; i++) {
        printf("%c", first_word[i]);
    }
    printf("*");
    for (i = 0; i < second_length; i++) {
        printf("%c", second_word[i]);
    }
    for (j = 0; j < max_board_length - first_length - second_length - 1; j++) {
        printf("*");
    }

    printf("\n");
}

int main() {
    char first_word[MAX_WORD_LENGTH];
    char second_word[MAX_WORD_LENGTH];


    if (!read_data()) return 1;

    if (!find_pair(first_word, second_word)) {
        printf("No such pair exists!\n");
        return 0;
    }

    display_board(first_word, second_word);
    return 0;
}

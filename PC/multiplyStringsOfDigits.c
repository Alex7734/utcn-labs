#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* multiplyStrings(char* num1, char* num2) {
    int len1 = strlen(num1);
    int len2 = strlen(num2);

    if (len1 == 0 || len2 == 0) {
        char* result = malloc(2);
        strcpy(result, "0");
        return result;
    }

    int resultSize = len1 + len2;
    char* result = (char*)malloc(resultSize + 1);
    memset(result, '0', resultSize);
    result[resultSize] = '\0';

    for (int i = len1 - 1; i >= 0; i--) {
        int carry = 0;
        for (int j = len2 - 1; j >= 0; j--) {
            int product = (num1[i] - '0') * (num2[j] - '0') + (result[i + j + 1] - '0') + carry;
            carry = product / 10;
            result[i + j + 1] = (product % 10) + '0';
        }
        result[i] += carry;
    }

    int i = 0;
    while (result[i] == '0') {
        i++;
    }

    if (i == resultSize) {
        char* finalResult = malloc(2);
        strcpy(finalResult, "0");
        return finalResult;
    }

    char* finalResult = malloc(resultSize - i + 1);
    strcpy(finalResult, result + i);
    free(result);

    return finalResult;
}

int main() {
    char num1[105], num2[105];

    printf("Enter the first number: ");
    scanf("%s", num1);

    printf("Enter the second number: ");
    scanf("%s", num2);

    char* product = multiplyStrings(num1, num2);

    printf("Product: %s\n", product);
    free(product);

    return 0;
}
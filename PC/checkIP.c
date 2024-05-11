#include <stdio.h>
#include <stdlib.h>
#include <string.h>
// Project headers
#include <exception.h>
#include <memoryLeaks.h>
#include <validateIPAddress.h>

int is_valid_ip(const char *ip) {
    // Check for invalid characters
    for (int i = 0; i < strlen(ip); i++) {
        if (ip[i] < '0' || ip[i] > '9') {
            if (ip[i] != '.') {
                return 0;
            }
        }
    }

    // Split the string into four parts
    int part1, part2, part3, part4;
    // Use this as comparison
    char buff[strlen(ip)];

    sscanf(ip, "%d.%d.%d.%d", &part1, &part2, &part3, &part4);

    // Create buffer of your parts + .s
    sprintf(buff, "%d.%d.%d.%d", part1, part2, part3, part4);

    // Check if ip has any extra unwanted chars
    if (strcmp(ip, buff) != 0){
        return 0;
    }

    // Check that each part is within the correct range
    return part1 >= 0 && part1 <= 255 &&
           part2 >= 0 && part2 <= 255 &&
           part3 >= 0 && part3 <= 255 &&
           part4 >= 0 && part4 <= 255;
}

unsigned validateIPAddress(const char *string) {

    if (string == NULL){
        setErrorInfo(NULL_POINTER);
        return 0;
    }

    setErrorInfo(OK);
    return is_valid_ip(string);
}

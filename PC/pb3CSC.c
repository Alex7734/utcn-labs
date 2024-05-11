
/*
 * Scrieți în C o funcție recursivă numită diferenta care primește ca și parametri un șir de numere reale (care
are cel puțin un element) și numărul de elemente ale acestuia și care returnează diferența dintre suma tuturor
elementelor aflate pe poziții pare și suma tuturor elementelor aflate pe poziții impare. Numerotarea pozițiilor în
tablou începe cu poziția zero.
 *
 */

double diferenta(double* list, int len){
    if (len == 1){
        return list[len-1] - list[len];
    }
    list[len-2] += list[len];
    return diferenta(list, len-1);
}

int diferentaMake(){
    double myArr[] = {30.5, -4.1, -20.3, 80.7};
    printf("%g", diferenta(myArr, 3));
    return 0;
}
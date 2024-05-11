#include <stdio.h>
#include <stdlib.h>

/*
 *
3.5. Să se introducă tipul "rațional" ca o structură formată din numărător şi numitor. Să
se scrie funcţii de simplificare, adunare, scădere, înmulţire, împărţire, ridicare la putere.
 *
 */

typedef struct {
    int numarator;
    int numitor;
} rational;

int gcd(int n, int m)
{
    int gcd, remainder;
    while (n != 0)
    {
        remainder = m % n;
        m = n;
        n = remainder;
    }
    gcd = m;
    return gcd;
}

void simplify(rational *fraction){
    if (fraction == NULL){
        printf("NULL address");
        return;
    }

    // Exception case II
    if (fraction->numitor == 0){
        printf("Division by 0");
        return;
    }


    if (fraction->numarator == 0){
        fraction->numitor = 1;
        return;
    }

    int gcdu = gcd(fraction->numarator, fraction->numitor);
    fraction->numarator = fraction->numarator / gcdu;
    fraction->numitor = fraction->numitor / gcdu;

    if (fraction->numarator<0 && fraction->numitor<0){
        fraction->numarator = -fraction->numarator;
        fraction->numitor = -fraction->numitor;
    }
    if (fraction->numitor<0 && fraction->numarator>0){
        fraction->numarator = -fraction->numarator;
        fraction->numitor = -fraction->numitor;
    }

}


void add(rational *a, rational *b, rational *c)
{
    c->numitor=a->numitor*b->numitor;
    c->numarator=a->numarator*b->numitor+b->numarator*a->numitor;
    simplify(c);
}

int main() {
    rational a,b,c;

    scanf("%d/%d\n%d/%d", &a.numarator, &a.numitor, &b.numarator, &b.numitor);
    add(&a, &b, &c);
    printf("%d/%d", c.numarator, c.numitor);

    return 0;
}

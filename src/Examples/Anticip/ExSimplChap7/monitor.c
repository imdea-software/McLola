#include <stdio.h>
#include <stdlib.h>
#include "engine.h"

/* Main program */

int main ()
{
    input *s = malloc(sizeof(input));
    int x, a, b;
    
    initialise();
    
    while (scanf("%d %d %d", &x, &a, &b) != EOF) {
        s->s = (_Bool) x;
        s->a = (_Bool) a;
        s->b = (_Bool) b;
        step(s);
    }
    
    finish();
    
    return 0;
}

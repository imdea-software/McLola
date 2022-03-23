#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "engine.h"

/* Main program */

int main ()
{
    input *s = malloc(sizeof(input));
    int x;
    
    initialise();
    
    while (scanf("%d", &x) != EOF) {
        s->a = (_Bool) x;
        step(s);
    }
    
    finish();
    
    return 0;
}

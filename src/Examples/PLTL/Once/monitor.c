#include <stdio.h>
#include <stdlib.h>
#include "engine.h"

/* Main program */

int main ()
{
    input *s = malloc(sizeof(input));
    int temp;
    
    initialise();
    
    while (scanf("%d", &temp) != EOF) {
        s->p = temp;
        step(s);
    }
    
    finish();
    
    return 0;
}

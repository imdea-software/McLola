#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "engine.h"

/* Main program */

int main ()
{
    input *s = malloc(sizeof(input));
    int x, y;

    // Initialisation
    initialise();
    
    // Input
    while ((scanf("%lf %lf %d %d ",
    &(s->p), &(s->q), &x, &y)) != EOF) {
        s->r = (_Bool) x;
        s->s = y;
        
        // Temporal execution
        step(s);
        
    }
    
    finish();
    
    return 0;
}


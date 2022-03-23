#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "engine.h"

/* Main program */

int main ()
{
    input *s = malloc(sizeof(input));
    int x;

    // Initialisation
    initialise();
    
    // Input
    while ((scanf("%lf %lf %d ",
    &(s->p), &(s->q), &x)) != EOF) {
        s->r = (_Bool) x;
        
        // Temporal execution
        step(s);
        
    }
    
    finish();
    
    return 0;
}


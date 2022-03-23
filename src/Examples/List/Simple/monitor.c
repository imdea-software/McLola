#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "engine.h"

/* Main program */

int main ()
{
    input *s = malloc(sizeof(input));
    
    // Initialisation
    initialise();
    
    while ((scanf("%lf %lf ",
    // Input
    &(s->p), &(s->q))) != EOF) {
        
        // Temporal execution
        step(s);
        
    }
    
    finish();
    
    return 0;
}


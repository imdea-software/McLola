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
    
    while ((scanf("%lf %lf %lf ",
    // Input
    &(s->s1), &(s->s2), &(s->s3))) != EOF) {
        
        // Temporal execution
        step(s);
        
    }
    
    finish();
    
    return 0;
}


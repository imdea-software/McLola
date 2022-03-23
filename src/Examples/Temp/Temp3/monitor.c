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
    
    // Spec 1
    while (scanf("%d %d",
    // Input
    &(s->a), &(s->b)) != EOF) {
        
        // Temporal execution
        step(s);
        
    }
    
    finish();
    
    return 0;
}

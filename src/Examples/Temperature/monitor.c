#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "engine.h"

/* Main program */

int main ()
{
    input *s = malloc(sizeof(input));
    
    initialise();
    
    while (scanf("%lf", &(s->temp)) != EOF)
        step(s);
    
    finish();
    
    return 0;
}

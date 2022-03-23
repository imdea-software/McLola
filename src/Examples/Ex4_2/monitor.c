#include <stdio.h>
#include <stdlib.h>
#include "engine.h"

/* Main program */

int main ()
{
    input *s = malloc(sizeof(input));
    
    initialise();
    
    while (scanf("%d", &(s->a)) != EOF)
        step(s);
    
    finish();
    
    return 0;
}

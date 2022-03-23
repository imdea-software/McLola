#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "engine.h"

/* Main program */

int main ()
{
    input *s = malloc(sizeof(input));
    
    initialise();
    
    while (scanf("%d %lf", &(s->a) , &(s->b)) != EOF)
        step(s);
    
    finish();
    
    return 0;
}

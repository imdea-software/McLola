#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "engine.h"

/* Main program */

int main ()
{
    input *s = malloc(sizeof(input));
    int i,n;
    
    // Initialisation
    initialise();
    
    // Spec 1
    while (scanf("%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %d",
    // Input
    &(s->attitude).yaw, &(s->attitude).roll, &(s->attitude).pitch,
    &(s->target).yt, &(s->target).xt, &(s->target).mutex, &(s->target).num_wp,
    &(s->velocity).y, &(s->velocity).x,
    &(s->position).yp, &(s->position).xp, &(s->position).alt, &(s->position).zone,
    &s->altitude,
    &(s->nofly).data[0].x, &(s->nofly).data[0].y,
    &(s->nofly).data[1].x, &(s->nofly).data[1].y,
    &(s->nofly).data[2].x, &(s->nofly).data[2].y,
    &(s->nofly).data[3].x, &(s->nofly).data[3].y,
    &(s->nofly).data[4].x, &(s->nofly).data[4].y,
    &n) != EOF) {
        // Complete input events
        i = 0;
        for (i;i<n;i++)
            scanf("%d ", &(s->events_within).data[i]);
        (s->events_within).size = 30;
        (s->events_within).n = n;
		
        // Complete input nofly
        (s->nofly).size = 30;
        (s->nofly).n = 5;
        
        // Temporal execution
        step(s);
        
    }
    
    finish();
    
    return 0;
}

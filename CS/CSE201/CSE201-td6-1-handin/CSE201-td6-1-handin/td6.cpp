#include <iostream>
#include "math.h"

#include "common.hpp"
#include "td6.hpp"


// Exercise 1: define the functions Target::get_status and Target::set_status


void Target::simulate_step(double simulation_interval)
{
    TargetStatus status_now = get_status();
    if (status_now == destroyable)
    {
        set_status(unbreakable);
    }
    if (status_now == unbreakable)
    {
        set_status(hidden);
    }
    if (status_now == hidden)
    {
        set_status(destroyable);
    }
}


#if EXERCISE_3 == 1
// Exercise 3: fix the implementation of halve_distance
Coordinate halve_distance(const Coordinate &c1, const Coordinate &c2) {
    Coordinate c;
    c.set_x((c1.get_x() + c2.get_x()) / 2);
    c.set_y((c1.get_y() + c2.get_y()) / 2);

    return c;
}
#endif



// Exercise 5: reduce the number of Coordinate instances
int count_half_segments(const Coordinate& start, const Coordinate& end, double min_distance)
{
    Coordinate m = halve_distance(start, end);
    if (m.get_distance(start) >= min_distance)
    {
        return 1 + count_half_segments(start, m, min_distance) + count_half_segments(m, end, min_distance);
    }
    else
    {
        return 0;
    }
}

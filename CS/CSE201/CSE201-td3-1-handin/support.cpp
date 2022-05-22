#include <iostream>     // std::cout, std::fixed
#include <iomanip>      // std::setprecision
#include <math.h>       // sin, cos
#include <limits>       // numeric_limits
#include <stdlib.h>     // include rand
#include <cstdlib>
#include "support.hpp"

namespace support {


double compute_distance(double x1,
                        double y1,
                        double x2,
                        double y2) {
  x1 = (x1 - x2);
  x1 = x1 * x1;
  y1 = (y1 - y2);
  y1 = y1 * y1;
  return sqrt(x1 + y1);
}


double* find_collision(const double x, const double y,
                       double *targets, const int num_targets) {
  double *found;
  found = targets;
  for (int i = 0; i < num_targets; ++i) {
    if (compute_distance(x,y,targets[i*2],targets[i*2+1]) <= 1.0) {
      return found;
    }
    found += 2;
  }
  return NULL;
}

bool intersect_obstacle(double x1, double y1,
                        const int i, const int j) {

  return (x1 >= (i * GRID_SIZE) &&
          x1 <= ((i+1) * GRID_SIZE) &&
          y1 >= (j * GRID_SIZE) &&
          y1 <= ((j+1) * GRID_SIZE));
}

int* find_collision(const double x, const double y,
                    int *obstacles, const int num_obstacles) {
  int *found;
  found = obstacles;

  for (int i = 0; i < num_obstacles; ++i) {
    if (intersect_obstacle(x,y,obstacles[i*2],obstacles[i*2+1])) {
      return found;
    }
    found += 2;
  }

  return NULL;
}

void remove_target(double* targets, int &tot_targets, double* target_to_remove) {
  double* last_element = targets + ((tot_targets-1)*2);

  for (double *ptr = target_to_remove; ptr != last_element;) {
    *ptr = *(ptr + 2);
    *(ptr + 1) = *(ptr + 3);
    ptr += 2;
  }
  tot_targets--;
}

}


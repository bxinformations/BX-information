#include <iostream>     // std::cout, std::fixed
#include <iomanip>      // std::setprecision
#include <math.h>       // sin, cos
#include <limits>       // numeric_limits
#include <stdlib.h>     // include rand
#include <cstdlib>
#include "td2.hpp"

void read_point(std::istream &in, // DO NOT CHANGE
                double &x,         // YOU CAN CHANGE THIS LINE
                double &y) {       // YOU CAN CHANGE THIS LINE
    in >> x; // DO NOT CHANGE
    in >> y; // DO NOT CHANGE
}

double compute_distance(double x1,   // YOU CAN CHANGE THIS LINE
                        double y1,   // YOU CAN CHANGE THIS LINE
                        double x2,   // YOU CAN CHANGE THIS LINE
                        double y2) { // YOU CAN CHANGE THIS LINE
  x1 = (x1 - x2);  // DO NOT CHANGE
  x1 = x1 * x1;    // DO NOT CHANGE
  y1 = (y1 - y2);  // DO NOT CHANGE
  y1 = y1 * y1;    // DO NOT CHANGE
  return sqrt(x1 + y1);  // DO NOT CHANGE
}


double td2_max(double first,  // YOU CAN CHANGE THIS LINE (apart from the function name td2_max)
            double second) {  // YOU CAN CHANGE THIS LINE
    if (first > second) {
        return first;
    } else {
        return second;
    }
}

void generate_target(double &x1, double &y1)
{
    x1 = rand() % 100;
    y1 = rand() % 100;
}

void generate_obstacle(int &i, int &j)
{
    i = rand() % 9;
    j = rand() % 9;
}

void generate_targets(double *targets, const int num_targets)
{
    for (int i = 0; i < num_targets; i++)
    {
        generate_target(targets[2 * i], targets[2 * i + 1]);
    }
}

void generate_obstacles(int *obstacles, const int num_obstacles)
{
    for (int i = 0; i < num_obstacles; i++)
    {
        generate_obstacle(obstacles[2 * i], obstacles[2 * i + 1]);
    }
}


void sort(double *targets, const int num_targets)
{
    for (int i = 0; i < num_targets; i++)
    {
        for (int j = i + 1; j < num_targets; j++)
        {
            if (targets[2 * i] > targets[2 * j])
            {
                std::swap(targets[2 * i], targets[2 * j]);
                std::swap(targets[2 * i + 1], targets[2 * j + 1]);
            }
        }
    }
}

void sort(int *obstacles, const int num_obstacles)
{
    for (int i = 0; i < num_obstacles; i++)
    {
        for (int j = i + 1; j < num_obstacles; j++)
        {
            if (obstacles[2 * i] > obstacles[2 * j])
            {
                std::swap(obstacles[2 * i], obstacles[2 * j]);
                std::swap(obstacles[2 * i + 1], obstacles[2 * j + 1]);
            }
        }
    }
}

double* find_collision(const double x, const double y,
                       double *targets, const int num_targets)
{
    for (int i = 0; i < num_targets; i++)
    {
        if (compute_distance(x, y, targets[2 * i], targets[2 * i + 1]) <= 1)
        {
            return targets + (2 * i);
        }
    }

  return NULL;
}

bool intersect_obstacle(double x1, double y1,
                        const int i, const int j)
{
    if ((x1 >= i * 10) && (x1 <= i * 10 + 10) && (y1 >= j * 10) && (y1 <= j * 10 + 10))
    {
        return true;
    }
    return false;
}


int* find_collision(const double x, const double y,
                    int *obstacles, const int num_obstacles)
{
    for (int i = 0; i < num_obstacles; i++)
    {
        if (intersect_obstacle(x, y, obstacles[2 * i], obstacles[2 * i + 1]))
        {
            return obstacles + (2 * i);
        }
    }

  return NULL;
}

void remove_target(double* targets, int &tot_targets, double* target_to_remove)
{
    int i = 0;
    for (; i < tot_targets; i++)
    {
        if (targets + 2 * i == target_to_remove)
        {
            break;
        }
    }
    for (int j = i; j < tot_targets - 1; j++)
    {
        std::swap(targets[2 * j], targets[2 * (j + 1)]);
        std::swap(targets[2 * j + 1], targets[2 * (j + 1) + 1]);
    }
    tot_targets -= 1;
}

bool simulate_projectile(const double magnitude, const double angle,
                         const double simulation_interval,
                         double *targets, int &tot_targets,
                         int *obstacles, int tot_obstacles)
{
    double PI = 3.14159265; // use these variables for PI and g
    double g = 9.8;

    double vx = magnitude * cos(angle * PI / 180);
    double vy = magnitude * sin(angle * PI / 180);

    double x = 0, y = 0;
    double t = simulation_interval;

    while (1)
    {
        if (find_collision(x, y, obstacles, tot_obstacles) != NULL)
        {
            return false;
        }
        double *p = find_collision(x, y, targets, tot_targets);
        if (p != NULL)
        {
            remove_target(targets, tot_targets, p);
            return true;
        }
        y = vy * t - 0.5 * g * t * t;
        if (y <= 0)
        {
            return false;
        }
        x = vx * t;
        t += simulation_interval;
    }
}

// The following function implement the main loop of the game --- nothing to do here
void game_loop(std::ostream &out, std::istream &in,
               const int max_projectiles,
               int *obstacles, const int num_obstacles,
               double *targets, const int num_targets) {
  int remaining_projectiles = max_projectiles;
  int remaining_targets = num_targets;

  while (remaining_projectiles > 0 && remaining_targets > 0) {
    double magnitude, angle;

    out << "Insert the magnitude and angle for the projectile: ";
    read_point(in, magnitude, angle);

    if (simulate_projectile(magnitude, angle, 0.01,
                            targets, remaining_targets,
                            obstacles, num_obstacles)) {
      out << "You hit one target!" << std::endl;
    } else {
      out << "You missed the target..." << std::endl;
    }

    remaining_projectiles--;
  }

  if (remaining_targets == 0) {
    out << "You won!" << std::endl;
  } else {
    out << "You lost!" << std::endl;
  }
}

void run_game(std::ostream &out, std::istream &in) {
  int tot_projectiles = 5;
  int num_obstacles = 3;
  int num_targets = 2;
  int obstacles[num_obstacles * 2];
  double targets[num_targets * 2];

  // Generate random targets
  generate_targets(targets, num_targets);
  sort(obstacles, num_targets);

  // Generate random obstacles
  generate_obstacles(obstacles, num_obstacles);
  sort(obstacles, num_obstacles);

  out << "List of obstacles:";
  for (int i = 0; i < num_obstacles; i++)
    out << " (" << obstacles[2*i]
        << "," << obstacles[2*i+1] << ")";
  out << std::endl;

  out << "List of targets:";
  for (int i = 0; i < num_targets; i++)
    out << " (" << targets[2*i]
        << "," << targets[2*i+1] << ")";
  out << std::endl;

  // Game loop
  game_loop(out,in,
            tot_projectiles,
            obstacles,
            num_obstacles,
            targets, num_targets);
}

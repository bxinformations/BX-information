#ifndef TD2_HPP
#define TD2_HPP

#include <iostream>

#define GRID_SIZE 10
#define MAX_GRID (10*10)

void read_point(std::istream &in, // DO NOT CHANGE
                double &x,         // YOU CAN CHANGE THIS LINE
                double &y);        // YOU CAN CHANGE THIS LINE

double compute_distance(double x1,  // YOU CAN CHANGE THIS LINE
                        double y1,  // YOU CAN CHANGE THIS LINE
                        double x2,  // YOU CAN CHANGE THIS LINE
                        double y2); // YOU CAN CHANGE THIS LINE

double td2_max(double first,  // YOU CAN CHANGE THIS LINE (apart from the function name td2_max)
            double second);   // YOU CAN CHANGE THIS LINE

void generate_target(double &x1, double &y1);

void generate_obstacle(int &i, int &j);

void generate_targets(double *targets, const int num_targets);

void generate_obstacles(int *obstacles, const int num_obstacles);

void sort(double *targets, const int num_targets);

void sort(int *obstacles, const int num_obstacles);

double* find_collision(const double x, const double y,
                       double *targets, const int num_targets);

bool intersect_obstacle(double x1, double y1,
                        const int i, const int j);

int* find_collision(const double x, const double y,
                    int *obstacles, const int num_obstacles);

void remove_target(double* targets, int &tot_targets, double* target_to_remove);

bool simulate_projectile(const double magnitude, const double angle,
                         const double simulation_interval,
                         double *targets, int &tot_targets,
                         int *obstacles, int tot_obstacles);

// This function is already implemented for you
void run_game(std::ostream &out, std::istream &in);

#endif // TD2_HPP

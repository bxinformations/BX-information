#ifndef SUPPORT_HPP
#define SUPPORT_HPP


namespace support {

#define GRID_SIZE 10
#define MAX_GRID (10*10)

double compute_distance(double x1,
                        double y1,
                        double x2,
                        double y2);

double* find_collision(const double x, const double y,
                       double *targets, const int num_targets);

bool intersect_obstacle(double x1, double y1,
                        const int i, const int j);

int* find_collision(const double x, const double y,
                    int *obstacles, const int num_obstacles);

void remove_target(double* targets, int &tot_targets, double* target_to_remove);

}

#endif // SUPPORT_HPP

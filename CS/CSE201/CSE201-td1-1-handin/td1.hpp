#ifndef TD1_HPP
#define TD1_HPP

#include <iostream>

double max(double first, double second);

int max_io(std::ostream &cout, std::istream &cin);

int read_doubles(std::ostream &cout, std::istream &cin);

double simulate_projectile(const double magnitude,
                           const double angle,
                           const double simulation_interval);

double compute_min_distance(const double magnitude,
                            const double angle,
                            const double simulation_interval,
                            const double x_target,
                            const double y_target);

double simulate_multiple_projectiles(const double shots_magnitude[],
                                     const double shots_angle[],
                                     const int total_projectile,
                                     const double simulation_interval,
                                     const double x_target,
                                     const double y_target);

int play_game(std::ostream &out, std::istream &in);

#endif // TD1_HPP

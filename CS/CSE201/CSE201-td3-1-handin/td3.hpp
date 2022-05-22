#ifndef TD3_HPP
#define TD3_HPP

double* extend_array(double* array, int length, int new_size);

double* shrink_array(double* array, int length, int new_size);


double* append_to_array(double element,
                        double* array,
                        int &current_size,
                        int &max_size);

double* remove_from_array(double* array,
                          int &current_size,
                          int &max_size);

bool simulate_projectile(const double magnitude, const double angle,
                         const double simulation_interval,
                         double *targets, int &tot_targets,
                         int *obstacles, int tot_obstacles,
                         double* &telemetry,
                         int &telemetry_current_size,
                         int &telemetry_max_size);

void merge_telemetry(double **telemetries,
                     int tot_telemetries,
                     int *telemetries_sizes,
                     double* &telemetry,
                     int &telemetry_current_size,
                     int &telemetry_max_size);

#endif // TD3_HPP

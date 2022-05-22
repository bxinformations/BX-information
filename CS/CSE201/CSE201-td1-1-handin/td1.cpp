#include <iostream>     // std::cout, std::fixed
#include <iomanip>      // std::setprecision
#include <math.h>       // sin, cos
#include <limits>       // numeric_limits

/**
 * @brief Computes the maximum between two numbers
 * @param first
 * @param second
 * @return the maximum between first and second
 */
double max(double first, double second)
{
    return first > second ? first : second;
}

/**
 * @brief Reads two numbers and output the maximum
 * @param cout Output stream to print the maximum
 * @param cin Input stream to read the input numbers
 * @return 0 if there are no errors
 */
int max_io(std::ostream &out, std::istream &in)
{
    double a, b;
    in >> a >> b;
    a = max(a, b);
    out << "The maximum number is:" << a << std::endl;

    // WARNING -- remember to output
    // "The maximum number is: " followed by the maximum number

    return 0;
}


/**
 * @brief read_doubles Read and prints a list of 5 numbers
 * @param cout Stream to print the numbers
 * @param cin Stream to read the numbers from
 * @return 0 if there are no errors
 */
int read_doubles(std::ostream &out, std::istream &in)
{
    double numbers[5];
    for (int i = 0; i < 5; i++)
        in >> numbers[i];
    for (int i = 0; i < 5; i++)
        out << numbers[i]<<" ";

    // WARNING -- remember to output
    // the list of numbers you read from in

    return 0;
}

/**
 * @brief Computes the trajectory of a projectile
 * @param magnitude of the initial velocity vector of the projectile
 * @param angle of the initial velocity vector of the projectile
 * @param simulation_interval time interval used to simulate the projectile's
 * trajectory
 * @return the final position of the projectile before hitting the ground
 */
double simulate_projectile(const double magnitude,
                           const double angle,
                           const double simulation_interval)
{
    double PI = 3.14159265; // use these variables for PI and g
    double g = 9.8;

    double vx = magnitude * cos(angle * PI / 180);
    double vy = magnitude * sin(angle * PI / 180);

    double x = 0, y = 0;
    double t = simulation_interval;

    while (1)
    {
        y = vy * t - 0.5 * g * t * t;
        if (y <= 0)
        {
            return x;
        }
        x = vx * t;
        t += simulation_interval;
    }
}


/**
 * @brief Computes the minimum distance between the projectile trajectory
 * and a target
 * @param magnitude of the initial velocity vector of the projectile
 * @param angle of the initial velocity vector of the projectile
 * @param simulation_interval time interval used to simulate the projectile's
 * trajectory
 * @param x_target is the x coordinate of the target
 * @param y_target is the y coordinate of the target
 * @return the minimum distance from the projectile to the target
 */
double compute_min_distance(const double magnitude,
                            const double angle,
                            const double simulation_interval,
                            const double x_target,
                            const double y_target) {

    double PI = 3.14159265; // use these variables for PI and g
    double g = 9.8;

    double vx = magnitude * cos(angle * PI / 180);
    double vy = magnitude * sin(angle * PI / 180);

    double x = 0, y = 0;
    double t = simulation_interval;

    double ans = 12345677;

    while ((t <= 100) && (y >= 0))
    {
        double  dis = sqrt((x - x_target) * (x - x_target) + (y - y_target) * (y - y_target));
        ans = -max(-ans, -dis);
        y = vy * t - 0.5 * g * t * t;
        x = vx * t;
        t += simulation_interval;
    }
    return ans;
}


/**
 * @brief Computes the minimum distance between several projectile
 * trajectories and a  target
 * @param proj_magitude magnitudes of the projectiles
 * @param proj_angle angles of the projectiles
 * @param simulation_interval time interval used to simulate the projectile's
 * trajectory
 * @param x_target is the x coordinate of the target
 * @param y_target is the y coordinate of the target
 * @return the minimum distance
 */
double simulate_multiple_projectiles(const double proj_magnitude[],
                                     const double proj_angle[],
                                     const int total_projectile,
                                     const double simulation_interval,
                                     const double x_target,
                                     const double y_target)
{
    double ans = 1234567;

    for (int i = 0; i < total_projectile; i++)
    {
        ans = -max(-ans, -compute_min_distance(proj_magnitude[i], proj_angle[i], simulation_interval, x_target, y_target));
    }

    return ans;
}


/**
 * @brief Shooting game
 * @param out is the output stream to print the game's output
 * @param in is the input stream to read the game's input
 * @return 0 if the function terminates correctly
 */
int play_game(std::ostream &out, std::istream &in)
{
    double simulation_interval = 0.05;

    double x_target, y_target;

    in >> x_target >> y_target;

    double proj_magnitude[5];
    double proj_angle[5];

    for (int i = 0; i < 5; i++)
    {
        in >> proj_magnitude[i] >> proj_angle[i];
    }

    double ans = simulate_multiple_projectiles(proj_magnitude, proj_angle, 5, simulation_interval, x_target, y_target);

    // WARNING -- remember to output
    // "You hit the target" if a projectile hit target
    // "You did not hit the target" if it didn't

    if (abs(ans) <= 1)
    {
        out << "You hit the target";
    }
    else
    {
        out << "You did not hit the target";
    }

    return 0;
}

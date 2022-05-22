#include <iostream>
#include <iomanip>
#include <sstream>
#include <cstdarg>
#include <iterator>
#include <string>
#include <regex>
#include <numeric>
#include <cmath>

#include "../gradinglib/gradinglib.hpp"

#include "td1.hpp"


namespace tdgrading {

using namespace testlib;
using namespace std;

int grading_max(ostream &out, const std::string test_name) {
    const std::string function_name = "max";

    start_test_suite(out, test_name);
    std::vector<int> res = {
        test_eq(out, function_name, max(0.0, 1.0), 1.0, 0.0, 1.0),
        test_eq(out, function_name, max(20.0, 1.0), 20.0, 20.0, 1.0),
        test_eq(out, function_name, max(-2.0, 1.0), 1.0, -2.0, 1.0),
        test_eq(out, function_name, max(-3.0, -4.0), -3.0, -3.0, -4.0),
        test_eq(out, function_name, max(0.0, 0.0), 0.0, 0.0, 0.0),
        test_eq(out, function_name, max(1.0, 1.0), 1.0, 1.0, 1.0)
    };
    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int grading_max_io(ostream &out, const std::string test_name) {
    const std::string function_name = "max_io";
    start_test_suite(out, test_name);

    std::vector<int> res = {
        test_in_output(out, function_name, max_io, "0.0 1.0", {R"(The maximum number is:1)"}),
        test_in_output(out, function_name, max_io, "20.0 1.0", {R"(The maximum number is:20)"}),
        test_in_output(out, function_name, max_io, "-2.0 1.0", {R"(The maximum number is:1)"}),
        test_in_output(out, function_name, max_io, "-3.0 -4.0", {R"(The maximum number is:-3)"}),
        test_in_output(out, function_name, max_io, "0.0 0.0", {R"(The maximum number is:0)"}),
        test_in_output(out, function_name, max_io, "1.0 1.1", {R"(The maximum number is:1.1)"})
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int grading_read_doubles(ostream &out, const std::string test_name) {
    const std::string function_name = "read_doubles";
    start_test_suite(out, test_name);

    std::vector<int> res = {
        test_in_output(out, function_name, read_doubles, "0.0 1.0 2.0 3.0 4.0", {R"(0 1 2 3 4)"}),
        test_in_output(out, function_name, read_doubles, "0.1 1.0 2.0 3.0 4.3", {R"(0.1 1 2 3 4.3)"})
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0),
                          res.size());
}

#define EQ_SIMULATE(magnitude, angle, simulation_interval, expected) \
  test_eq_approx(out, function_name, \
    simulate_projectile(magnitude, angle, simulation_interval), \
    expected, 0.1, \
    magnitude, angle, simulation_interval)

int grading_simulate_projectile(ostream &out, const std::string test_name) {
    const std::string function_name = "simulate_projectile";

    start_test_suite(out, test_name);

    std::vector<int> res = {
      // Corner cases, should be 0 (or close to)
      EQ_SIMULATE(0.000000,0.000000,0.050000,0.000000),
      EQ_SIMULATE(0.000000,2.000000,0.050000,0.000000),
      EQ_SIMULATE(20.000000,90.000000,0.050000,0.000000),
      EQ_SIMULATE(1.000000,20.000000,0.050000,0.046985),
      // standard behavior
      EQ_SIMULATE(25.000000,20.000000,0.050000,39.936936),
      EQ_SIMULATE(45.000000,20.000000,0.050000,131.087121),
      EQ_SIMULATE(45.000000,70.000000,0.050000,132.361795),
      // Test simulation interval (last point differs)
      EQ_SIMULATE(20.0,70.0,0.5,23.9414),
      EQ_SIMULATE(20.0,70.0,0.25,25.6515),
      EQ_SIMULATE(20.0,70.0,0.05,25.9935308),

    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0),
                          res.size());
}

#define EQ_DISTANCE(magnitude, angle, simulation_interval, \
                    xtarget, ytarget, expected)            \
  test_eq_approx(out, function_name,\
    compute_min_distance(magnitude, angle, simulation_interval, \
                         xtarget, ytarget), \
    expected, 0.01, \
    magnitude, angle, simulation_interval, xtarget, ytarget)


int grading_compute_min_distance(ostream &out, const std::string test_name) {
    const std::string function_name = "compute_min_distance";

    start_test_suite(out, test_name);

    std::vector<int> res = {
      EQ_DISTANCE( 0.0, 0.0, 0.05,  0.0, 0.0,  0.0),
      EQ_DISTANCE( 0.0, 2.0, 0.05,  0.0, 0.0,  0.0),
      EQ_DISTANCE( 0.0, 0.0, 0.05, 10.0, 0.0, 10.0),
      //
      EQ_DISTANCE( 25.0, 20.0, 0.05,  39.936936, 0.0, 0.374856),
      EQ_DISTANCE( 45.0, 20.0, 0.05, 131.087121, 0.0, 0.62281),
      EQ_DISTANCE( 45.0, 70.0, 0.05, 132.361795, 0.0, 1.25704),
      //
      EQ_DISTANCE( 20.0, 70.0, 0.500, 15.39, 17.4799, 1.783),
      EQ_DISTANCE( 20.0, 70.0, 0.250, 15.39, 17.4799, 0.0),
      EQ_DISTANCE( 20.0, 70.0, 0.005, 15.39, 17.4799, 0.0),
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0),
                          res.size());
}

// inlining for 5-size arrays - just faster to use the test
double simulate_multiple_projectiles_proxy(const double magnitude_0,
                                           const double magnitude_1,
                                           const double magnitude_2,
                                           const double magnitude_3,
                                           const double magnitude_4,
                                           const double angle_0,
                                           const double angle_1,
                                           const double angle_2,
                                           const double angle_3,
                                           const double angle_4,
                                           const double delta,
                                           const double x_target,
                                           const double y_target) {
  double magnitudes[5] = {magnitude_0, magnitude_1, magnitude_2, magnitude_3, magnitude_4};
  double angles[5] = {angle_0, angle_1, angle_2, angle_3, angle_4};

  return simulate_multiple_projectiles(magnitudes, angles, 5, delta, x_target, y_target);
}

// WORKAROUND to print the function params
#define EQ_MUL_PROJ(m1,m2,m3,m4,m5,a1,a2,a3,a4,a5,delta, x_target, y_target, expected) \
  test_eq_approx(out, function_name, \
                 simulate_multiple_projectiles_proxy(m1,m2,m3,m4,m5,a1,a2,a3,a4,a5, delta, x_target, y_target), \
                 expected, 0.1, \
                 to_string(m1) + "," + to_string(m2) + "," + to_string(m3) + "," + to_string(m4) + "," + to_string(m5) , \
                 to_string(a1) + "," + to_string(a2) + "," + to_string(a3) + "," + to_string(a4) + "," + to_string(a5) , \
                 5, delta, x_target, y_target)

int grading_multiple_projectiles(ostream &out, const std::string test_name) {
    const std::string function_name = "multiple_projectile";

    start_test_suite(out, test_name);

    std::vector<int> res = {
      EQ_MUL_PROJ(20,30,40,50,60,20,20,20,20,20,0.05,5.000000,5.000000,3.064070),
      EQ_MUL_PROJ(20,30,40,50,60,45,45,45,45,45,0.05,5.000000,5.000000,0.303315),
      EQ_MUL_PROJ(20,30,40,50,60,70,70,70,70,70,0.05,5.000000,5.000000,2.783200),
      EQ_MUL_PROJ(20,30,40,50,60,20,20,20,20,20,0.05,10.000000,10.000000,6.224025),
      EQ_MUL_PROJ(20,30,40,50,60,45,45,45,45,45,0.05,10.000000,10.000000,0.628800),
      EQ_MUL_PROJ(20,30,40,50,60,70,70,70,70,70,0.05,10.000000,10.000000,4.842173),
      EQ_MUL_PROJ(20,30,40,50,60,20,20,20,20,20,0.05,15.000000,15.000000,9.479962),
      EQ_MUL_PROJ(20,30,40,50,60,45,45,45,45,45,0.05,15.000000,15.000000,0.765990),
      EQ_MUL_PROJ(20,30,40,50,60,70,70,70,70,70,0.05,15.000000,15.000000,2.355182),
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0),
                          res.size());
}

// Use almost the same input as EQ_MUL_PROJ, but expected is the string expected in the output
// so we can reuse the same test cases
#define TEST_PLAY_GAME_HELPER(m1,m2,m3,m4,m5,a1,a2,a3,a4,a5,delta,x_target,y_target, expected) \
  test_in_output(out, function_name, play_game, \
                 to_string(x_target) + " " + to_string(y_target) + " " + \
                 to_string(m1) + " " + to_string(a1) + " " +             \
                 to_string(m2) + " " + to_string(a2) + " " +             \
                 to_string(m3) + " " + to_string(a3) + " " +             \
                 to_string(m4) + " " + to_string(a4) + " " +             \
                 to_string(m5) + " " + to_string(a5),                    \
                 expected)

int grading_play_game(ostream &out, const std::string test_name) {
    const std::string function_name = "play_game";

    start_test_suite(out, test_name);

    std::vector<int> res = {
      // hit the target
      TEST_PLAY_GAME_HELPER(20,30,40,50,60,45,45,45,45,45,0.05,5.000000,5.000000,{"You hit the target"}),
      TEST_PLAY_GAME_HELPER(20,30,40,50,60,45,45,45,45,45,0.05,10.000000,10.000000,{"You hit the target"}),
      TEST_PLAY_GAME_HELPER(20,30,40,50,60,45,45,45,45,45,0.05,15.000000,15.000000,{"You hit the target"}),
      // Don't hit the target
      TEST_PLAY_GAME_HELPER(20,30,40,50,60,20,20,20,20,20,0.05,5.000000,5.000000,{"You did not hit the target"}),
      TEST_PLAY_GAME_HELPER(20,30,40,50,60,70,70,70,70,70,0.05,5.000000,5.000000,{"You did not hit the target"}),
      TEST_PLAY_GAME_HELPER(20,30,40,50,60,20,20,20,20,20,0.05,10.000000,10.000000,{"You did not hit the target"}),
      TEST_PLAY_GAME_HELPER(20,30,40,50,60,70,70,70,70,70,0.05,10.000000,10.000000,{"You did not hit the target"}),
      TEST_PLAY_GAME_HELPER(20,30,40,50,60,20,20,20,20,20,0.05,15.000000,15.000000,{"You did not hit the target"}),
      TEST_PLAY_GAME_HELPER(20,30,40,50,60,70,70,70,70,70,0.05,15.000000,15.000000,{"You did not hit the target"}),
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0),
                          res.size());
}


int grading(std::ostream &out, const int test_case_number)
{
/**

Annotations used for the autograder.

[START-AUTOGRADER-ANNOTATION]
{
  "total" : 7,
  "names" : ["max", "max_io", "read_doubles", "simulate_projectiles", "compute_min_distance", "multiple_projectiles", "play_game"],
  "points" : [5,5,10,30,20,20,10]
}
[END-AUTOGRADER-ANNOTATION]
*/

    int const total_test_cases = 7;
    std::string const test_names[total_test_cases] = {"max",
                                                      "max_io",
                                                      "read_doubles",
                                                      "simulate_projectiles",
                                                      "compute_min_distance",
                                                      "multiple_projectiles",
                                                      "play_game"};
    int const points[total_test_cases] = {5,5,10,30,20,20,10};
    int (*test_functions[total_test_cases]) (std::ostream &, const std::string) = {grading_max,
                                                                                   grading_max_io,
                                                                                   grading_read_doubles,
                                                                                   grading_simulate_projectile,
                                                                                   grading_compute_min_distance,
                                                                                   grading_multiple_projectiles,
                                                                                   grading_play_game};
    return run_grading(out, test_case_number, total_test_cases,
                       test_names, points,
                       test_functions);
}

} // End of namepsace tdgrading

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
#include "td2.hpp"
#include <limits>

namespace tdgrading {

using namespace testlib;
using namespace std;


bool test_read_point(std::ostream &out,
                     const double first,
                     const double second) {
  double first_var = numeric_limits<double>::max();
  double second_var = numeric_limits<double>::max();

  std::string instring = to_string(first) + " " + to_string(second);
  std::istringstream tmpin(instring);

  read_point(tmpin, first_var, second_var);

  bool success = (first == first_var && second == second_var);
  std::string successstring = success ? get_success() : get_failure();
  (out << successstring) << " ";
  print_tested_function_input(out, "read_point", instring);

  out << ": got first = " << first_var << " second = " << second_var
      << " expected first = " << first << " second = " << second
      << std::endl;

  return success;
}

bool test_compute_distance(std::ostream &out,
                           const double x1,
                           const double y1,
                           const double x2,
                           const double y2) {
  double app_x1, app_y1, app_x2, app_y2;
  app_x1 = x1;
  app_y1 = y1;
  app_x2 = x2;
  app_y2 = y2;

  compute_distance(app_x1, app_y1, app_x2, app_y2);

  bool success = (x1 == app_x1 && x2 == app_x2 &&
                  y1 == app_y1 && y2 == app_y2);
  out << (success ? testlib::get_success() : testlib::get_failure()) << " ";

  print_tested_function(out, "compute_distance", x1, y1, x2, y2);

  {
    out << ": got x1 = "
        << app_x1
        << ",y1 = "
        << app_y1
        << ",x2 = "
        << app_x2
        << ",y2 = "
        << app_y2
        << " expected x1 = "
        << x1
        << ",y1 = "
        << y1
        << ",x2 = "
        << x2
        << ",y2 = "
        << y2
        << std::endl;
  }

  return success;
}


template <typename T, typename... Arguments>
bool test_eq_range(std::ostream &out,
                   const std::string &function_name,
                   void (*f) (T &first, T &second),
                   T first, T second,
                   T lower_bound,
                   T upper_bound) {
    bool success;
    T first_c = first;
    T second_c = second;

    f(first_c, second_c);

    // May not work on some types.
    success = (first_c >= lower_bound && first_c <= upper_bound) &&
      (second_c >= lower_bound && second_c <= upper_bound);

    out << (success ? "[SUCCESS] " : "[FAILURE] ");

    print_tested_function(out, function_name, first, second);

    out << ": got " << first_c << " and " << second_c
        << " expected values in the range [" << lower_bound << "," << upper_bound << "]";
    out << std::endl;

    return success;
}

template <typename T>
bool test_eq_range_array(std::ostream &out,
                         const std::string &function_name,
                         void (*f) (T* array, const int tot_elements),
                         int tot_elements,
                         T lower_bound,
                         T upper_bound) {
    bool success;
    T array[tot_elements*2];
    T o1,o2;

    for(int i = 0; i < tot_elements; ++i) {
      array[2*i] = lower_bound - 1.0;
      array[2*i+1] = lower_bound - 1.0;
    }

    f(array, tot_elements);

    // May not work on some types.
    success = true;
    for(int i = 0; i < tot_elements; ++i) {
      success = success &&
        (array[2*i] >= lower_bound && array[2*i] <= upper_bound) &&
        (array[2*i+1] >= lower_bound && array[2*i+1] <= upper_bound);
      if (! success) {
        o1 = array[2*i];
        o2 = array[2*i+1];
        break;
      }
    }

    out << (success ? "[SUCCESS] " : "[FAILURE] ");

    print_tested_function(out, function_name);

    if (success) {
      out << ": got all elements in the range";
    } else {
      out << ": got " << o1 << " and " << o2;
    }
    out << " expected values in the range [" << lower_bound << "," << upper_bound << "]";
    out << std::endl;

    return success;
}

template <typename T>
bool test_sorted(std::ostream &out,
                 const std::string &function_name,
                 void (*sort) (T* array, const int tot_elements),
                 const T* to_sort,
                 int tot_elements) {
    bool success;
    T copy[tot_elements*2];
    T o1,o2;

    for(int i = 0; i < tot_elements; ++i) {
      copy[i*2] = to_sort[i*2];
      copy[i*2+1] = to_sort[i*2+1];
    }

    sort(copy, tot_elements);

    // May not work on some types.
    success = true;
    for(int i = 1; i < tot_elements; ++i) {
      success = success && (copy[2*(i-1)] <= copy[2*i]);
      if (! success) {
        o1 = copy[2*(i-1)];
        o2 = copy[2*i];
        break;
      }
    }

    out << (success ? "[SUCCESS] " : "[FAILURE] ");
    out << function_name << "([";
    for(int i = 0; i < tot_elements*2; ++i) {
      if (i != 0) out << ", ";
      out << to_sort[i];
    }
    out << "], " << tot_elements << ")";

    if (success) {
      out << ": got all elements are sorted";
    } else {
      out << ": got " << o1 << " > " << o2;
    }
    out << " expected a sorted array";
    out << std::endl;

    return success;
}



int test_function_signature(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

    std::vector<int> res = {
        test_read_point(out, 1, 2),
        test_read_point(out, 3.0, 4.0),
        //
        test_compute_distance(out, 1.0, 1.0, 4.0, 4.0),
        test_compute_distance(out, -2.0, -3.0, 4.0, 25.0),
        //
        test_eq(out, "td_max", td2_max(0, 1), 1, 0, 1),
        test_eq(out, "td_max", td2_max(2, 1.2), 2, 2, 1.2),
        test_eq(out, "td_max", ((double) td2_max(1.2, 3.4)), 3.4, 1.2, 3.4),
        test_eq(out, "td_max", ((double) td2_max(5.1, 1.2)), 5.1, 5.1, 1.2)
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int test_random_generators(std::ostream &out, const std::string test_name) {
    double x,y;
    int i,j;

    x = -1;
    y = -1;
    i = -1;
    j = -1;

    start_test_suite(out, test_name);

    std::vector<int> res = {
      test_eq_range(out, "generate_target", generate_target, x, y, 0.0, 100.0),
      test_eq_range(out, "generate_target", generate_target, x, y, 0.0, 100.0),
      test_eq_range(out, "generate_target", generate_target, x, y, 0.0, 100.0),
      //
      test_eq_range(out, "generate_obstacle", generate_obstacle, i, j, 0, 10),
      test_eq_range(out, "generate_obstacle", generate_obstacle, i, j, 0, 10),
      test_eq_range(out, "generate_obstacle", generate_obstacle, i, j, 0, 10)
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int test_random_generators_array(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

    std::vector<int> res = {
      test_eq_range_array(out, "generate_targets", generate_targets, 10, 0.0, 100.0),
      test_eq_range_array(out, "generate_targets", generate_targets, 100, 0.0, 100.0),
      test_eq_range_array(out, "generate_targets", generate_targets, 100, 0.0, 100.0),
      //
      test_eq_range_array(out, "generate_obstacles", generate_obstacles, 10, 0, 10),
      test_eq_range_array(out, "generate_obstacles", generate_obstacles, 100, 0, 10),
      test_eq_range_array(out, "generate_obstacles", generate_obstacles, 100, 0, 10)
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int test_sort(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

    double n2_1[] = {1.0, 1.0, 2.0, 2.0};
    double n2_2[] = {2.0, 2.0, 1.0, 1.0};
    double n2_3[] = {2.0, 1.0, 1.0, 2.0};
    double n3_1[] = {2.0, 1.0, 0.0, 2.0, 2.0, 2.0, 1.3, 2.0};

    int i2_1[] = {1, 1, 2, 2};
    int i2_2[] = {2, 2, 1, 1};
    int i2_3[] = {2, 1, 1, 2};
    int i3_1[] = {2, 1, 0, 2, 2, 2, 1, 2};

    std::vector<int> res = {
      test_sorted(out, "sort", sort, n2_1, 2),
      test_sorted(out, "sort", sort, n2_2, 2),
      test_sorted(out, "sort", sort, n2_3, 2),
      test_sorted(out, "sort", sort, n3_1, 3),
      //
      test_sorted(out, "sort", sort, i2_1, 2),
      test_sorted(out, "sort", sort, i2_2, 2),
      test_sorted(out, "sort", sort, i2_3, 2),
      test_sorted(out, "sort", sort, i3_1, 3)
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int test_collision_target(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

    // Input must be sorted by x coordinate
    double* t1_0 = (double*) NULL;
    double t1_1[] = {0.0, 0.0};
    double t2_1[] = {0.0, 0.0, 1.0, 0.0};
    double t2_2[] = {1.0, 0.0, 5.0, 0.0};
    double t2_3[] = {1.0, 1.0, 5.0, 0.0};
    double t3_1[] = {1.0, 0.0, 5.0, 0.0, 5.0, 0.0};

    std::vector<int> res = {
      test_eq_ptr(out, "find_collision",
                  find_collision(0.0,0.0,t1_0,0), ( (double *) NULL)),
      test_eq_ptr(out, "find_collision",
                  find_collision(0.0,0.0,t1_1,1), t1_1),
      test_eq_ptr(out, "find_collision",
                  find_collision(0.0,0.0,t2_1,2), t2_1),
      test_eq_ptr(out, "find_collision",
                  find_collision(5.0,0.0,t2_2,2), (t2_2+2)),
      test_eq_ptr(out, "find_collision",
                  find_collision(5.0,0.0,t3_1,3), (t3_1+2)),
      test_eq_ptr(out, "find_collision",
                  find_collision(20.0,0.0,t2_2,2), ( (double *) NULL)),
      test_eq_ptr(out, "find_collision",
                  find_collision(0.0,0.0,t2_3,2), ( (double *) NULL))
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}


#define OBST_TC(x,y,i,j,res) \
  test_eq(out, "intersect_obstacle",          \
          intersect_obstacle(x,y,i,j), res,   \
          x,y,i,j)
int test_intersect_obstacle(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

    std::vector<int> res = {
      OBST_TC(0,0,0,0,true),
      OBST_TC(1,1,0,0,true),
      OBST_TC(0,1,0,0,true),
      OBST_TC(1,0,0,0,true),
      OBST_TC(10.01,0,0,0,false),
      OBST_TC(1,10.01,0,0,false),
      //
      OBST_TC(9.9,0,1,1,false),
      OBST_TC(20.1,0,1,1,false),
      OBST_TC(0.0,9.9,1,1,false),
      OBST_TC(0.0,20.1,1,1,false),
      //
      OBST_TC(40,30,4,3,true),
      OBST_TC(50,30,4,3,true),
      OBST_TC(50.01,30,4,3,false),
      OBST_TC(50.01,30,4,3,false),
      //
      OBST_TC(53.55,46.8697,5,4,true)
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}


int test_collision_obstacles(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

    // Input must be sorted by x coordinate
    int* null_ptr = ( (int *) NULL);
    int* t1_0 = (int*) NULL;
    int t1_1[] = {0, 0};

    int t3_1[] = {0, 0, 7, 8, 9, 9};


    std::vector<int> res = {
      test_eq_ptr(out, "find_collision", find_collision(0,0,t1_0,0), null_ptr),
      test_eq_ptr(out, "find_collision", find_collision(0,0,t1_1,1), t1_1),
      test_eq_ptr(out, "find_collision", find_collision(0.2,0.2,t1_1,1), t1_1),
      test_eq_ptr(out, "find_collision", find_collision(20.1,0.2,t1_1,1), null_ptr),
      //
      test_eq_ptr(out, "find_collision", find_collision(20.1,0.2,t3_1,3), null_ptr),
      test_eq_ptr(out, "find_collision", find_collision(75.0,0.2,t3_1,3), null_ptr),
      test_eq_ptr(out, "find_collision", find_collision(75.0,85.0,t3_1,3), t3_1+2),
      test_eq_ptr(out, "find_collision", find_collision(81.0,85.0,t3_1,3), null_ptr),
      test_eq_ptr(out, "find_collision", find_collision(90.0,92.0,t3_1,3), t3_1+4),

    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int test_target_aux(std::ostream &out,
                    double *vector, int tot_targets, double* to_remove) {
  bool success = true;
  double new_vector[tot_targets*2];
  double expected[tot_targets*2];
  double *new_to_remove;
  int new_tot_targets = tot_targets;

  for (int i = 0; i < tot_targets; i++) {
    new_vector[2*i] = vector[2*i];
    new_vector[2*i+1] = vector[2*i+1];
  }

  {
    double *ptr;
    int j;
    ptr = vector;
    j = 0;
    for (int i = 0; i < tot_targets; i++) {
      if (ptr != to_remove) {
        expected[2*j] = *ptr;
        expected[2*j+1] = *(ptr+1);
        j++;
      } else {
        new_to_remove = new_vector + (2*j);
      }
      ptr += 2;
    }
  }

  remove_target(new_vector, new_tot_targets, new_to_remove);

  success = success && new_tot_targets == (tot_targets - 1);
  for (int i = 0; success && i < tot_targets - 1; i++) {
    success = success && new_vector[2*i] == expected[2*i] &&
      new_vector[2*i+1] == expected[2*i+1];
  }

  out << (success ? "[SUCCESS] " : "[FAILURE] ");
  out << "remove_target(";
  print_array(out, vector, tot_targets*2);
  out << ", " << tot_targets
      << ", " << *to_remove;
  out << "): got ";
  print_array(out, new_vector, new_tot_targets*2);
  out << " expected ";
  print_array(out, expected, (tot_targets-1)*2);
  out << std::endl;

  return success;
}

int test_remove_target(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

    double t3_1[] = {1.0, 1.0, 5.0, 5.0, 10.0, 100.0};
    double t3_2[] = {1.0, 1.0, 5.0, 5.0, 5.0, 100.0};

    std::vector<int> res = {
      test_target_aux(out, t3_1, 3, t3_1),
      test_target_aux(out, t3_1, 3, t3_1+2),
      test_target_aux(out, t3_1, 3, t3_1+4),
      test_target_aux(out, t3_2, 3, t3_2),
      test_target_aux(out, t3_2, 3, t3_2+2),
      test_target_aux(out, t3_2, 3, t3_2+4)
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int test_simulate_projectile(std::ostream &out, const std::string test_name) {
    std::string fun_name = "simulate_projectile";

    int projectiles[3][2] = {{40,60},{35,60},{20,70}};

    double targets_0[] = {80,60};
    int num_targets_0 = 1;
    int *obstacles_0 = (int *) NULL;

    double targets_1[] = {80,60,80,36};
    int num_targets_1 = 2;
    int *obstacles_1 = (int *) NULL;

    double targets_2[] = {80,60,80,36};
    int num_targets_2 = 2;
    int obstacles_2[] = {5,4,5,7,3,6};
    int num_obstacles_2 = 3;

    start_test_suite(out, test_name);

    std::vector<int> res = {
      test_eq(out, fun_name,
              simulate_projectile(projectiles[0][0],projectiles[0][1],
                                  0.01, targets_0, num_targets_0, obstacles_0, 0),
              true,
              projectiles[0][0], projectiles[0][1], 0.01, targets_0, 1, obstacles_0, 0),
      test_eq(out, fun_name,
              simulate_projectile(projectiles[1][0],projectiles[1][1],0.01, targets_0, num_targets_0, obstacles_0, 0),
              false,
              projectiles[1][0], projectiles[1][1],0.01, targets_0, 1, obstacles_0, 0),
      test_eq(out, fun_name,
              simulate_projectile(projectiles[2][0],projectiles[2][1],0.01, targets_0, num_targets_0, obstacles_0, 0),
              false,
              projectiles[2][0], projectiles[2][1],0.01, targets_0, 1, obstacles_0, 0),
      //
      test_eq(out, fun_name,
              simulate_projectile(projectiles[0][0],projectiles[0][1],0.01, targets_1, num_targets_1, obstacles_1, 0),
              true,
              projectiles[0][0], projectiles[0][1],0.01, targets_1, 1, obstacles_1, 0),
      test_eq(out, fun_name,
              simulate_projectile(projectiles[1][0],projectiles[1][1],0.01, targets_1, num_targets_1, obstacles_1, 0),
              true,
              projectiles[1][0], projectiles[1][1],0.01, targets_1, 1, obstacles_1, 0),
      test_eq(out, fun_name,
              simulate_projectile(projectiles[2][0],projectiles[2][1],0.01, targets_1, num_targets_1, obstacles_1, 0),
              false,
              projectiles[2][0], projectiles[2][1],0.01, targets_1, 1, obstacles_1, 0),
      //
      test_eq(out, fun_name,
              simulate_projectile(projectiles[0][0],projectiles[0][1],0.01, targets_2, num_targets_2, obstacles_2, 0),
              true,
              projectiles[0][0], projectiles[0][1],0.01, targets_2, 1, obstacles_2, num_obstacles_2),
      test_eq(out, fun_name,
              simulate_projectile(projectiles[1][0],projectiles[1][1],0.01, targets_2, num_targets_2, obstacles_2, num_obstacles_2),
              false,
              projectiles[1][0], projectiles[1][1],0.01, targets_2, 1, obstacles_2, num_obstacles_2),
      test_eq(out, fun_name,
              simulate_projectile(projectiles[2][0],projectiles[2][1],0.01, targets_1, num_targets_1, obstacles_1, 0),
              false,
              projectiles[2][0], projectiles[2][1],0.01, targets_1, 1, obstacles_1, 0),
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int grading(std::ostream &out, const int test_case_number)
{
/**

Annotations used for the autograder.

[START-AUTOGRADER-ANNOTATION]
{
  "total" : 9,
  "names" : ["function_signature",
             "random",
             "generate_multiple",
             "test_sort",
             "test_collision_target",
             "test_intersect_obstacle",
             "test_collision_obstacles",
             "test_remove_target",
             "test_simulate_projectile"],
  "points" : [10,5,5,20,10,5,10,20,15]
}
[END-AUTOGRADER-ANNOTATION]
*/

    int const total_test_cases = 9;
    std::string const test_names[total_test_cases] = {"function_signature",
                                                      "random",
                                                      "generate_multiple",
                                                      "test_sort",
                                                      "test_collision_target",
                                                      "test_intersect_obstacle",
                                                      "test_collision_obstacles",
                                                      "test_remove_target",
                                                      "test_simulate_projectile"};
    int const points[total_test_cases] = {10,5,5,20,10,5,10,20,15};
    int (*test_functions[total_test_cases]) (std::ostream &, const std::string) = {
      test_function_signature,
      test_random_generators,
      test_random_generators_array,
      test_sort,
      test_collision_target,
      test_intersect_obstacle,
      test_collision_obstacles,
      test_remove_target,
      test_simulate_projectile,
    };

    return run_grading(out, test_case_number, total_test_cases,
                       test_names, points,
                       test_functions);
}

} // End of namepsace tdgrading

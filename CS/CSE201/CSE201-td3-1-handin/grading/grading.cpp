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
#include "memplumber.hpp"

#include "td3.hpp"
#include <limits>

namespace tdgrading {

using namespace testlib;
using namespace std;


template <typename... Arguments>
bool test_extend_array_aux(std::ostream &out,
                           const std::string func_name,
                           double* (*f) (double*, int, int),
                           double* original,
                           int length,
                           int new_size,
                           const Arguments&... args) {
  // Test:
  // All elements should be copied in the new array
  // The memory of the previous array must be freed
  // The program must allocate exactly the required memory

  bool success;
  std::string failure_string;
  double expected[new_size];
  double result[new_size];
  double *copy, *extended;
  size_t memLeakCount;
  uint64_t memLeakSize;

  size_t mem_leaks_count_post;
  uint64_t  mem_leaks_size_post;

  int errors = 0;

  for(int i = 0; i < new_size; ++i) {
    if (i < length)
      expected[i] = original[i];
    else
      expected[i] = 0;
  }

  // start the test case
  success = true;
  MemPlumber::start();

  // memory still allocated after the function
  mem_leaks_size_post = sizeof(double) * new_size;
  mem_leaks_count_post = 1;

  copy = new double[length];
  for(int i = 0; i < length; ++i) {
    copy[i] = original[i];
  }

  extended = f(copy, length, new_size);
  MemPlumber::memLeakCheck(memLeakCount, memLeakSize, false);

  if (mem_leaks_count_post != memLeakCount ||
      mem_leaks_size_post != memLeakSize) {
    // did not allocate enough memory or did not free the element;
    success = false;
    errors = 1;
  }
  if (success && extended == NULL && new_size != 0) {
    success = false;
    errors = 2;
  }
  if (success && extended != NULL) {
    for (int i = 0; i < new_size; ++i) {
      if (i < length && extended[i] != original[i]) {
        success = false; // the array was not copied
        errors = 3;
        break;
      }
    }

    for(int i = 0; i < new_size; ++i)
      result[i] = extended[i];

    delete[] extended;
  }

  MemPlumber::stopAndFreeAllMemory();

  out << (success ? "[SUCCESS] " : "[FAILURE] ");

  print_tested_function(out, func_name, args...);

  if (success) {
    out << ": got ";
    print_array(out, result, new_size);
  } else {
    out << ": got ";

    switch (errors) {
    case 1:
      out << "your function ";
      if (memLeakCount != mem_leaks_count_post) {
        if (memLeakCount > mem_leaks_count_post)
          out << "has not enough delete operations";
        else
          out << "has not enough new operations";

        out << "# new - # delete is ";
        out << (memLeakCount - mem_leaks_count_post) << " instead of 0 ";
      }

      if (memLeakSize != mem_leaks_size_post) {
        if (memLeakCount != mem_leaks_count_post)
          out << "and ";
        if (memLeakSize > mem_leaks_size_post)
          out << "uses more memory than expected";
        else
          out << "does not use enough memory";
        out << " (" << memLeakSize << " bytes instead of "
            << mem_leaks_size_post << " bytes) ";
      }
      break;
    case 2:
      out << "NULL";
      break;
    case 3:
      out << "at least one element was not copied in the result array";
      break;
    default:
      out << "unknown error";
      break;
    }
  }
  out << " expected ";
  print_array(out, expected, new_size);
  out << std::endl;

  return success;
}

template <typename... Arguments>
bool test_append_array_aux(std::ostream &out,
                           const std::string func_name,
                           double* (*f) (double, double*, int&, int&),
                           double new_element,
                           double* original,
                           int size,
                           int max,
                           const Arguments&... args) {
  bool success;
  int current_size, max_size;
  int reason = 0;
  double expected[max+5];
  double *copy = new double[max];

  success = true;
  current_size = size;
  max_size = max;

  for (int i = 0; i < max_size; ++i) expected[i] = original[i];
  expected[size] = new_element;

  for (int i = 0; i < size; ++i) copy[i] = original[i];
  copy = f(new_element, copy, current_size, max_size);

  if (copy == NULL) {
    // return a null array
    success = false;
    reason = 1;
  } else {
    if (current_size != size + 1) {
      // did not update the size
      success = false;
      reason = 2;
    } else if (size == max && max_size != max + 5) {
      // did not extend the array correctly
      success = false;
      reason = 3;
    } else if (size != max && max_size != max) {
      // updated max when not necessary
      success = false;
      reason = 4;
    } else {
      for (int i = 0; i < size + 1; ++i) {
        if (i < size + 1 && copy[i] != expected[i]) {
          // did not copy
          success = false;
          reason = 5;
          break;
        }
      }
    }
  }

  out << (success ? "[SUCCESS] " : "[FAILURE] ");
  print_tested_function(out, func_name, args...);

  out << ": got ";
  if (! success && reason > 1) {
    if (reason == 2) out << current_size << " as the array size ";
    else if (reason == 3) out << max_size << " as the array max_size ";
    else if (reason == 4) out << max_size << " as the array max_size ";
    out << "and ";
  }
  if (copy == NULL) out << "NULL ";
  else print_array(out, copy, current_size);

  out << " expected ";
  print_array(out, expected, size + 1);
  out << std::endl;

  if (NULL != copy) delete[] copy;

  return success;
}

template <typename... Arguments>
bool test_remove_from_array_aux(std::ostream &out,
                                const std::string func_name,
                                double* (*f) (double*, int&, int&),
                                double* original,
                                int size,
                                int max,
                                const Arguments&... args) {
  bool success;
  int current_size, max_size;
  int reason = 0;
  double expected[max];
  double *copy = new double[max];

  success = true;

  current_size = size;
  max_size = max;
  for (int i = 0; i < size - 1; ++i) expected[i] = original[i];

  for (int i = 0; i < size; ++i) copy[i] = original[i];
  copy = f(copy, current_size, max_size);

  if (copy == NULL) {
    // return a null array
    success = false;
    reason = 1;
  } else {
    if ((current_size != size - 1 && size > 0) ||
        (current_size != 0 && size == 0)) {
      // did not update the size
      success = false;
      reason = 2;
    } else if ( (size == 0 && max_size != max) ||
                (size > 0 && size - 1 <= max - 5 && max_size != max - 5) ||
                (size > 0 && size - 1 > max - 5 && max_size != max)) {
      // did not extend the array correctly
      success = false;
      reason = 3;
    } else if (size - 1 > max - 5 && max_size != max) {
      // updated max when not necessary
      success = false;
      reason = 4;
    } else {
      for (int i = 0; i < size - 1; ++i) {
        if (i < size - 1 && copy[i] != expected[i]) {
          // did not copy
          success = false;
          reason = 5;
          break;
        }
      }
    }
  }


  out << (success ? "[SUCCESS] " : "[FAILURE] ");
  print_tested_function(out, func_name, args...);

  out << ": got ";
  if (! success && reason > 1) {
    if (reason == 2) out << current_size << " as the array size ";
    else if (reason == 3) out << max_size << " as the array max_size ";
    else if (reason == 4) out << max_size << " as the array max_size ";
    out << "and ";
  }
  if (copy == NULL) out << "NULL ";
  else print_array(out, copy, current_size);

  out << " expected ";
  print_array(out, expected, size - 1);
  out << std::endl;

  if (NULL != copy) delete[] copy;

  return success;
}

int test_extend_array(std::ostream &out, const std::string test_name) {
    std::string fun_name = "extend_array";

    double t1[] = {1.0,2.0,3.0};

    start_test_suite(out, test_name);

    std::vector<int> res = {
      test_extend_array_aux(out, "extend_array", extend_array,
                            t1, 2, 4, "[1,2]", 2, 3),
      test_extend_array_aux(out, "extend_array", extend_array,
                            t1, 1, 5, "[1]", 1, 5),
      test_extend_array_aux(out, "extend_array", extend_array,
                            t1, 0, 4, "[]", 0, 4),
      test_extend_array_aux(out, "extend_array", extend_array,
                            t1, 0, 0, "[]", 0, 0),
      test_extend_array_aux(out, "extend_array", extend_array,
                            t1, 2, 2, "[1,2]", 2, 2)
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int test_shrink_array(std::ostream &out, const std::string test_name) {
    std::string fun_name = "shrink_array";

    double t1[] = {1.0,2.0,3.0,4.0,5.0};

    start_test_suite(out, test_name);

    std::vector<int> res = {
      test_extend_array_aux(out, "shrink_array", shrink_array,
                            t1, 5, 2, "[1,2,4,5]", 5, 2),
      test_extend_array_aux(out, "shrink_array", shrink_array,
                            t1, 5, 5, "[1,2,4,5]", 5, 5),
      test_extend_array_aux(out, "shrink_array", shrink_array,
                            t1, 5, 0, "[1,2,4,5]", 5, 0),
      test_extend_array_aux(out, "shrink_array", shrink_array,
                            t1, 0, 0, "[]", 0, 0)
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}


int test_append_to_array(std::ostream &out, const std::string test_name) {
    std::string fun_name = "append_to_array";
    double t1[] = {0,1,2,3,4,5};

    start_test_suite(out, test_name);

    std::vector<int> res = {
      test_append_array_aux(out, fun_name, append_to_array,
                            7, t1, 6, 6,
                            7, "[0,1,2,3,4,5]", 6, 6),
      test_append_array_aux(out, fun_name, append_to_array,
                            7, t1, 3, 6,
                            7, "[0,1,2]", 6, 6),
      test_append_array_aux(out, fun_name, append_to_array,
                            7, t1, 0, 6,
                            7, "[]", 1, 6),
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int test_remove_from_array(std::ostream &out, const std::string test_name) {
    std::string fun_name = "remove_to_array";

    double t1[] = {0,1,2,3,4,5};

    start_test_suite(out, test_name);

    std::vector<int> res = {
      test_remove_from_array_aux(out, fun_name, remove_from_array,
                                 t1, 6, 6,
                                 "[0,1,2,3,4,5]", 6, 6),
      test_remove_from_array_aux(out, fun_name, remove_from_array,
                                 t1, 2, 6,
                                 "[0,1]", 2, 6),
      test_remove_from_array_aux(out, fun_name, remove_from_array,
                                 t1, 1, 4,
                                 "[0]", 1, 4),
      test_remove_from_array_aux(out, fun_name, remove_from_array,
                                 t1, 1, 5,
                                 "[0]", 1, 5),
      test_remove_from_array_aux(out, fun_name, remove_from_array,
                                 t1, 0, 3,
                                 "[]", 0, 3),
      test_remove_from_array_aux(out, fun_name, remove_from_array,
                                 t1, 0, 4,
                                 "[]", 0, 4)
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}


bool test_simulate_projectile_aux(std::ostream &out,
                                  const double magnitude, const double angle,
                                  const double simulation_interval,
                                  double *targets, int &tot_targets,
                                  int *obstacles, int tot_obstacles,
                                  const bool res_expected,
                                  const int telemetry_size_expected) {
  bool res, success;

  double* telemetry;
  int telemetry_current_size;
  int telemetry_max_size;

  telemetry_current_size = 0;
  telemetry_max_size = 5;
  telemetry = new double[telemetry_max_size];

  res = simulate_projectile(magnitude, angle,
                            simulation_interval,
                            targets, tot_targets,
                            obstacles, tot_obstacles,
                            telemetry,
                            telemetry_current_size,
                            telemetry_max_size);

  delete[] telemetry;

  success = (res == res_expected &&
             telemetry_size_expected == telemetry_current_size);

  out << (success ? "[SUCCESS] " : "[FAILURE] ");
  print_tested_function(out, "simulate_projectile", magnitude, angle, simulation_interval,
                        targets, tot_targets, obstacles, tot_obstacles);

  out << ": got " << (res ? "true" : "false") << " as result and " << telemetry_current_size << " elements in the telemetry";
  out << " expected " << (res_expected ? "true" : "false") << " as result and " << telemetry_size_expected << " elements in the telemetry";
  out << std::endl;

  return success;
}

bool test_merge_telemetry_aux(std::ostream &out,
                              double **telemetries,
                              int tot_telemetries,
                              int *telemetries_sizes,
                              std::string expected_str) {
  bool success;
  int reason = 0;

  double* telemetry;
  int telemetry_current_size;
  int telemetry_max_size;

  telemetry_current_size = 0;
  telemetry_max_size = 5;
  telemetry = new double[telemetry_max_size];

  merge_telemetry(telemetries,
                  tot_telemetries,
                  telemetries_sizes,
                  telemetry,
                  telemetry_current_size,
                  telemetry_max_size);

  success = true;

  // TODO: check the telemetry size
  int tot_elements = 0;
  for (int i = 0; i < tot_telemetries; ++i)
    tot_elements += telemetries_sizes[i];
  if (telemetry_current_size != tot_elements) {
    success = false;
    reason = 1;
  }

  // check order
  for (int i = 0; i + 3 < telemetry_current_size; i = i + 3) {
    if (telemetry[i] > telemetry[i+3]) {
      success = false;
      reason = 2;
    }
  }

  // Check that the telemetry is the correct one
  // It should be robust if "sorting" is not stable
  if (success) {
    int indexes[tot_telemetries];

    for (int i = 0; i < tot_telemetries; ++i)
      indexes[i] = 0;

    for (int i = 0; i < telemetry_current_size;) {
      bool found = false;

      for (int j = 0; j < tot_telemetries && ! found; ++j) {
        if (indexes[j] < telemetries_sizes[j]) {
          if ((telemetries[j][indexes[j]] == telemetry[i] &&
               telemetries[j][indexes[j]+1] == telemetry[i+1] &&
               telemetries[j][indexes[j]+2] == telemetry[i+2])) {
            found = true;
            i = i + 3; // skip to the next element
            indexes[j] = indexes[j] + 3;
          }
        }
      }

      if (! found) {
        success = false;
        reason = 3;
        break;
      }
    }
  }

  delete[] telemetry;

  out << (success ? "[SUCCESS] " : "[FAILURE] ");
  print_tested_function(out, "merge_telemetry",
                        telemetries, tot_telemetries, telemetries_sizes);

  out << ": got ";
  if (! success) {
    if (reason == 1) out << "the wrong number of elements ";
    else if (reason == 2) out << "an unsorted telemetry ";
    else if (reason == 3) out << "a wrong telemetry ";
    out << "and ";
  }
  print_array(out, telemetry, telemetry_current_size);
  out << " expected " << expected_str;

  out << std::endl;

  return success;
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
      test_simulate_projectile_aux(out, projectiles[0][0],projectiles[0][1],0.01, targets_0, num_targets_0, obstacles_0, 0, true, 1191),
      test_simulate_projectile_aux(out, projectiles[1][0],projectiles[1][1],0.01, targets_0, num_targets_0, obstacles_0, 0, false, 1857),
      test_simulate_projectile_aux(out, projectiles[2][0],projectiles[2][1],0.01, targets_0, num_targets_0, obstacles_0, 0, false, 1152),
      //
      test_simulate_projectile_aux(out, projectiles[0][0],projectiles[0][1],0.01, targets_1, num_targets_1, obstacles_1, 0, true, 1191),
      test_simulate_projectile_aux(out, projectiles[1][0],projectiles[1][1],0.01, targets_1, num_targets_1, obstacles_1, 0, true, 1365),
      test_simulate_projectile_aux(out, projectiles[2][0],projectiles[2][1],0.01, targets_1, num_targets_1, obstacles_1, 0, false, 1152),
      //
      test_simulate_projectile_aux(out, projectiles[0][0],projectiles[0][1],0.01, targets_2, num_targets_2, obstacles_2, 0, true, 1191),
      test_simulate_projectile_aux(out, projectiles[1][0],projectiles[1][1],0.01, targets_2, num_targets_2, obstacles_2, num_obstacles_2,false, 861),
      test_simulate_projectile_aux(out, projectiles[2][0],projectiles[2][1],0.01, targets_1, num_targets_1, obstacles_1, 0 ,false, 1152),
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int test_merge_telemetry(std::ostream &out, const std::string test_name) {
    double t1[] = {0,0,0,5,1,1};           //  6
    double t2[] = {0,0,0,2,3,3,2.5,6,7};   //  9
    double t3[] = {0,0,0,1.2,1,1};         //  6
    double t4[] = {0,0,0,3,1,1,10,20,20};  //  9
    double t5[] = {};                      //  0

    double* telemetries0[] = {};
    int tot_telemetries0 = 0;
    int telemetries_sizes0[] = {};

    double* telemetries1[] = {t1};
    int tot_telemetries1 = 1;
    int telemetries_sizes1[] = {6};

    double* telemetries2[] = {t1,t2};
    int tot_telemetries2 = 2;
    int telemetries_sizes2[] = {6,9};

    double* telemetries3[] = {t1,t2,t3,t4,t5};
    int tot_telemetries3 = 5;
    int telemetries_sizes3[] = {6,9,6,9,0};

    std::vector<int> res = {
      test_merge_telemetry_aux(out, telemetries0, tot_telemetries0, telemetries_sizes0, "[]"),
      test_merge_telemetry_aux(out, telemetries1, tot_telemetries1, telemetries_sizes1, "[0, 0, 0, 5, 1, 1]"),
      test_merge_telemetry_aux(out, telemetries2, tot_telemetries2, telemetries_sizes2, "[0, 0, 0, 0, 0, 0, 2, 3, 3, 2.5, 6, 7, 5, 1, 1]"),
      test_merge_telemetry_aux(out, telemetries3, tot_telemetries3, telemetries_sizes3, "[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.2, 1, 1, 2, 3, 3, 2.5, 6, 7, 3, 1, 1, 5, 1, 1, 10, 20, 20]"),
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
  "total" : 6,
  "names" : ["test_extend_array","test_shrink_array","test_append_to_array","test_remove_from_array","test_simulate_projectile","test_merge_telemetry"],
  "points" : [10,10,10,10,10,10]
}
[END-AUTOGRADER-ANNOTATION]
*/

    int const total_test_cases = 6;
    std::string const test_names[total_test_cases] = {"test_extend_array",
                                                      "test_shrink_array",
                                                      "test_append_to_array",
                                                      "test_remove_from_array",
                                                      "test_simulate_projectile",
                                                      "test_merge_telemetry"};
    int const points[total_test_cases] = {10,10,10,10,10,10};
    int (*test_functions[total_test_cases]) (std::ostream &, const std::string) = {
      test_extend_array,
      test_shrink_array,
      test_append_to_array,
      test_remove_from_array,
      test_simulate_projectile,
      test_merge_telemetry,
    };


    return run_grading(out, test_case_number, total_test_cases,
                       test_names, points,
                       test_functions);
}

} // End of namepsace tdgrading

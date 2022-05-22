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
#include "td7.hpp"
#include "grading.hpp"
#include "common.hpp"
#include "ComplexDouble.hpp"
#include "coordinatelist.hpp"
#include <limits>

namespace tdgrading {

using namespace testlib;
using namespace std;


int test_complex(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_1 == 1
  Complex<double> c_double(1.3, 2.3);
  Complex<float> c_float(1.3, 2.3);
  Complex<int> c_int(1, 2);

  std::vector<int> res = {
    test_eq(out, "Complex<double> c1a(1.3, 2.3).get_r", c_double.get_r(), 1.3),
    test_eq(out, "Complex<double> c1a(1.3, 2.3).get_i", c_double.get_i(), 2.3),
    test_eq(out, "Complex<float> c1a(1.3, 2.3).get_r", c_float.get_r(), (float) 1.3),
    test_eq(out, "Complex<float> c1a(1.3, 2.3).get_i", c_float.get_i(), (float) 2.3),
    test_eq(out, "Complex<int> c1a(1, 2).get_r", c_int.get_r(), 1),
    test_eq(out, "Complex<int> c1a(1, 2).get_i", c_int.get_i(), 2),
  };
#else
  std::vector<int> res = {
  };
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}


int test_nodelist(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_2 == 1
  auto t1 = [](ostream &out) {
    std::string description = "c1 = Coordinate(1,2); ListNode<Coordinate> n1 = ListNode<Coordinate>(c1);";
    Coordinate c1 = Coordinate(1,2);
    ListNode<Coordinate> n1 = ListNode<Coordinate>(c1);
    return (test_eq(out, description + " n1.get_next", n1.get_next(), (ListNode<Coordinate>*) NULL) &&
            test_eq(out, description + " n1.get_element().get_position().get_x",
                    n1.get_element(), c1));
  };

  auto t2 = [](ostream &out) {
    std::string description = "";
    int success;

    description += "c1 = Coordinate(1,2); ";
    description += "c2 = Coordinate(2,3); ";
    description += "ListNode<Coordinate> *n1 = new ListNode(c1); ";
    description += "ListNode<Coordinate> *n2 = new ListNode(c2); ";
    description += "n1->set_next(n2);";

    Coordinate c1 = Coordinate(1,2);
    Coordinate c2 = Coordinate(2,3);
    ListNode<Coordinate>* n1 = new ListNode<Coordinate>(c1);
    ListNode<Coordinate>* n2 = new ListNode<Coordinate>(c2);
    n1->set_next(n2);

    success = (test_eq(out, description + " n1->get_next", n1->get_next(), n2));

    delete n1;
    delete n2;

    return success;
  };

  std::vector<int> res = {
    t1(out),
    t2(out)
  };
#else
  std::vector<int> res = {
  };
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}

int test_list(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_3 == 1
  std::vector<Coordinate> pvec;
  for (int i = 0; i < 5; ++i) {
    Coordinate c(i,i+1);
    pvec.push_back(c);
  }

  std::vector<int> res = {
    test_list(out, "Coordinate", pvec, 0),
    test_list(out, "Coordinate", pvec, 1),
    test_list(out, "Coordinate", pvec, 2),
    test_list(out, "Coordinate", pvec, 5),
  };
#else
  std::vector<int> res = {
  };
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}


int test_reverse_even(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_4 == 1
  auto test_vector = [](ostream &out, vector<int> input, vector<int> expected) {
    std::string input_str = get_vector_str("int", input);
    vector<int> result = reverse_even(input);
    std::string result_str = get_vector_str("int", result);
    std::string expected_str = get_vector_str("int", expected);

    bool success = result == expected;
    out << (success ? "[SUCCESS] " : "[FAILURE] ");

    print_tested_function(out, "reverse_even", input_str);

    out << ": got " << result_str
        << " expected " << expected_str;
    out << std::endl;

    return success;
  };

  std::vector<int> res = {
    test_vector(out, std::vector<int>{}, std::vector<int>{}),
    test_vector(out, std::vector<int>{1}, std::vector<int>{}),
    test_vector(out, std::vector<int>{2}, std::vector<int>{2}),
    test_vector(out, std::vector<int>{1,2}, std::vector<int>{2}),
    test_vector(out, std::vector<int>{1,2,3,4}, std::vector<int>{4,2})
  };
#else
  std::vector<int> res = {
  };
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}

int test_same_coordinates(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_5 == 1
  auto test_vector = [](ostream &out,
                        vector<double> list_of_x,
                        vector<double> list_of_y,
                        vector<Coordinate> expected) {
    std::string list_of_x_str = get_vector_str("double", list_of_x);
    std::string list_of_y_str = get_vector_str("double", list_of_y);
    std::string expected_str = get_vector_str("Coordinate", expected);

    vector<Coordinate> result = same_coordinates(list_of_x, list_of_y);
    std::string result_str = get_vector_str("Coordinate", result);

    bool success = result == expected;
    out << (success ? "[SUCCESS] " : "[FAILURE] ");

    print_tested_function(out, "same_coordinate", list_of_x_str, list_of_y_str);

    out << ": got " << result_str
        << " expected " << expected_str;
    out << std::endl;

    return success;
  };

  std::vector<int> res = {
    test_vector(out, std::vector<double>{}, std::vector<double>{}, std::vector<Coordinate>{}),
    test_vector(out, std::vector<double>{1}, std::vector<double>{}, std::vector<Coordinate>{}),
    test_vector(out, std::vector<double>{}, std::vector<double>{1}, std::vector<Coordinate>{}),
    test_vector(out, std::vector<double>{1}, std::vector<double>{2}, std::vector<Coordinate>{}),
    test_vector(out, std::vector<double>{1}, std::vector<double>{1}, std::vector<Coordinate>{Coordinate(1,1)}),
    test_vector(out, std::vector<double>{1,2}, std::vector<double>{1}, std::vector<Coordinate>{Coordinate(1,1)}),
    test_vector(out, std::vector<double>{1}, std::vector<double>{1,2}, std::vector<Coordinate>{Coordinate(1,1)}),
    test_vector(out, std::vector<double>{1,1}, std::vector<double>{1,2}, std::vector<Coordinate>{Coordinate(1,1),Coordinate(1,1)}),
    test_vector(out, std::vector<double>{1,1}, std::vector<double>{1,1}, std::vector<Coordinate>{Coordinate(1,1),Coordinate(1,1),Coordinate(1,1),Coordinate(1,1)}),
    test_vector(out, std::vector<double>{1,2,3}, std::vector<double>{1,3}, std::vector<Coordinate>{Coordinate(1,1),Coordinate(3,3)}),
    test_vector(out, std::vector<double>{1,2,3,4,5}, std::vector<double>{1,3,4}, std::vector<Coordinate>{Coordinate(1,1),Coordinate(3,3),Coordinate(4,4)}),

  };
#else
  std::vector<int> res = {
  };
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}


int test_filter_max(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_6 == 1
  auto test_vector = [](ostream &out,
                        vector<int> max_vector,
                        vector<int> other_vector,
                        vector<int> expected) {
    std::string max_vector_str = get_vector_str("int", max_vector);
    std::string other_vector_str = get_vector_str("int", other_vector);
    std::string expected_str = get_vector_str("int", expected);

    vector<int> result = filter_max(max_vector, other_vector);
    std::string result_str = get_vector_str("int", result);

    bool success = result == expected;
    out << (success ? "[SUCCESS] " : "[FAILURE] ");

    print_tested_function(out, "filter_max", max_vector_str, other_vector_str);

    out << ": got " << result_str
        << " expected " << expected_str;
    out << std::endl;

    return success;
  };

  std::vector<int> res = {
    test_vector(out, std::vector<int>{}, std::vector<int>{}, std::vector<int>{}),
    test_vector(out, std::vector<int>{5}, std::vector<int>{}, std::vector<int>{}),
    test_vector(out, std::vector<int>{5}, std::vector<int>{1,5,6,7,8}, std::vector<int>{1,6,7,8}),
    test_vector(out, std::vector<int>{1,9,0},
                std::vector<int>{25,9,8,7,5,3,2},
                std::vector<int>{2,3,5,7,8,25}),
  };
#else
  std::vector<int> res = {
  };
#endif

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
  "names" : ["test_complex",
             "test_nodelist",
             "test_list",
             "test_reverse_even",
             "test_same_coordinates",
             "test_filter_max"],
  "points" : [10, 15, 15, 15, 10, 10]
}
[END-AUTOGRADER-ANNOTATION]
*/

    int const total_test_cases = 6;
    std::string const test_names[total_test_cases] = {"test_complex",
                                                      "test_nodelist",
                                                      "test_list",
                                                      "test_reverse_even",
                                                      "test_same_coordinates",
                                                      "test_filter_max"};
    int const points[total_test_cases] = {10, 15, 15, 15, 10, 10};
    int (*test_functions[total_test_cases]) (std::ostream &, const std::string) = {
      test_complex,
      test_nodelist,
      test_list,
      test_reverse_even,
      test_same_coordinates,
      test_filter_max,
    };

    return run_grading(out, test_case_number, total_test_cases,
                       test_names, points,
                       test_functions);
}

} // End of namepsace tdgrading

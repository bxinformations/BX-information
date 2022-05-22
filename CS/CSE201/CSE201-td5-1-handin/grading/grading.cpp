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
#include "td5.hpp"
#include <limits>

namespace tdgrading {

using namespace testlib;
using namespace std;


int test_overload_arithmetic(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_1 == 1
  auto test_overload_arithmetic_aux = [](std::ostream &out, int x1, int y1, int x2, int y2) {
    Coordinate c1(x1,y1);
    Coordinate c2(x2,y2);

    std::string descr_c1 = "Coordinate c1(" + std::to_string(x1) + "," + std::to_string(y1) + "); ";

    std::string descr = (descr_c1 +
                         "Coordinate c2(" + std::to_string(x2) + "," + std::to_string(y2) + "); ");

    return (
            test_eq(out, descr + "(c1 + c2).get_x", (c1 + c2).get_x(), x1 + x2) +
            test_eq(out, descr + "(c1 + c2).get_y", (c1 + c2).get_y(), y1 + y2) +
            //
            test_eq(out, descr_c1 + "(-c1).get_x", (-c1).get_x(), -x1) +
            test_eq(out, descr_c1 + "(-c1).get_y", (-c1).get_y(), -y1) +
            ///
            test_eq(out, descr + "(c1 - c2).get_x", (c1 - c2).get_x(), x1 - x2) +
            test_eq(out, descr + "(c1 - c2).get_y", (c1 - c2).get_y(), y1 - y2)
            );
  };

  std::vector<int> res = {
    test_overload_arithmetic_aux(out,0,0,0,0),
    test_overload_arithmetic_aux(out,0,0,1,1),
    //
    test_overload_arithmetic_aux(out,1,1,2,2),
    test_overload_arithmetic_aux(out,2,2,1,1),
    //
    test_overload_arithmetic_aux(out,1,2,1,2),
    test_overload_arithmetic_aux(out,1,1,1,1),
    test_overload_arithmetic_aux(out,1,1,1,0),
    test_overload_arithmetic_aux(out,1,0,1,0),
    //
    test_overload_arithmetic_aux(out,2,1,1,2),
    test_overload_arithmetic_aux(out,1,2,2,1)
  };
#else
  std::vector<int> res = {};
#endif
  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size() * 6);
}

int test_overload_comparisons(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_2 == 1
  auto test_overload_comparisons_aux = [](std::ostream &out, int x1, int y1, int x2, int y2) {
    Coordinate c1(x1,y1);
    Coordinate c2(x2,y2);

    std::string descr_c1 = "Coordinate c1(" + std::to_string(x1) + "," + std::to_string(y1) + "); ";
    std::string descr = (descr_c1 +
                         "Coordinate c2(" + std::to_string(x2) + "," + std::to_string(y2) + "); ");

    return (
            test_eq(out, descr, c1 == c2, (x1 == x2 && y1 == y2), "c1 == c2") +
            test_eq(out, descr, c1 != c2, (x1 != x2 || y1 != y2), "c1 != c2") +
            //
            test_eq(out, descr, c1 > c2, (x1 > x2 && y1 > y2), "c1 > c2") +
            test_eq(out, descr, c1 < c2, (x1 < x2 && y1 < y2), "c1 < c2") +
            0
            );
  };

  std::vector<int> res = {
    test_overload_comparisons_aux(out,0,0,0,0),
    test_overload_comparisons_aux(out,0,0,1,1),
    //
    test_overload_comparisons_aux(out,1,1,2,2),
    test_overload_comparisons_aux(out,2,2,1,1),
    //
    test_overload_comparisons_aux(out,1,2,1,2),
    test_overload_comparisons_aux(out,1,1,1,1),
    test_overload_comparisons_aux(out,1,1,1,0),
    test_overload_comparisons_aux(out,1,0,1,0),
    //
    test_overload_comparisons_aux(out,2,1,1,2),
    test_overload_comparisons_aux(out,1,2,2,1)
  };
#else
  std::vector<int> res = {};
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size() * 4);
}

int coordinates_static_fields(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_3 == 1
  auto test_static_fields_aux = [](std::ostream &out,
                                   std::vector<pair<double,double>> coordinates) {
    bool success = true;
    double x_max, y_max;
    std::string description;
    std::string expected;
    std::string res;
    x_max = 0;
    y_max = 0;

    auto check_res = [](bool &success,
                        std::string &result,
                        std::string &expected,
                        int x, int y, int x_exp, int y_exp) {
      success = x == x_exp;
      success = success && y == y_exp;
      result.assign("x = " + std::to_string(x) + " y = " + std::to_string(y));
      expected.assign("x = " + std::to_string(x_exp) + " y = " + std::to_string(y_exp));
    };

    Coordinate::reset_max();
    description += "Coordinate::reset_max();";
    for (auto iter = coordinates.begin();
         iter != coordinates.end() && success;
         ++iter) {
      auto pair = *iter;

      Coordinate(get<0>(pair), get<1>(pair));
      description += " Coordinate(" + std::to_string(get<0>(pair)) + "," + std::to_string(get<1>(pair)) + ");";

      if (get<0>(pair) > x_max) x_max = get<0>(pair);
      if (get<1>(pair) > y_max) y_max = get<1>(pair);
    }

    check_res(success, res, expected,
              Coordinate::get_x_max(), Coordinate::get_y_max(),
              x_max, y_max);

    out << (success ? "[SUCCESS] " : "[FAILURE] ");
    print_tested_function(out, "", description);
    out << ": got " << res << " expected " << expected;
    out << std::endl;

    return success;
  };


  std::vector<int> res = {
    // Reset max should set x and y to 0
    test_static_fields_aux(out, {}),
    // after adding one coordinate we get the max for x and max for y
    test_static_fields_aux(out, {pair<double,double>(1,2)}),
    // after adding two coordinate we get the max x from one and the max y for the other
    test_static_fields_aux(out, {pair<double,double>(3,1), pair<double,double>(2,4)}),
    test_static_fields_aux(out, {pair<double,double>(2,4), pair<double,double>(3,1)}),
    //
    test_static_fields_aux(out, {pair<double,double>(1,2), pair<double,double>(2,3), pair<double,double>(4,5)}),
    test_static_fields_aux(out, {pair<double,double>(4,5), pair<double,double>(2,3), pair<double,double>(1,2)}),
    test_static_fields_aux(out, {pair<double,double>(1,2), pair<double,double>(4,5), pair<double,double>(2,3)})
  };
#else
  std::vector<int> res = {};
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}






int projectile_inheritance(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_4 == 1
  auto test_projectile = [](ostream& out,
                            const double x, const double y,
                            const double magnitude, const double angle,
                            const double simulation_interval) {
    std::string description = "";
    Projectile reference(Coordinate(x,y), magnitude, angle);
    double last_x = reference.get_position().get_x();
    double max_x = reference.get_position().get_x();
    double max_y = reference.get_position().get_y();

    DroppingProjectile dropping(Coordinate(x,y), magnitude, angle);

    description += "DroppingProjectile dropping = DroppingProjectile(Coordinate(" + std::to_string(x) + "," + std::to_string(y) + "),";
    description += std::to_string(angle) + ", " + std::to_string(magnitude) + "); ";
    description += "simulate_full_trajectory(std::cerr, " + std::to_string(simulation_interval) + ", dropping);}";

    bool save_last = true;
    while (dropping.get_position().get_y() >= 0) {
      if (reference.get_velocity_y() > 0) {
        assert(reference.get_position() == dropping.get_position());
        assert(reference.get_velocity_x() == dropping.get_velocity_x());
        assert(reference.get_velocity_y() == dropping.get_velocity_y());
      } else {
        save_last = false;
      }

      if (reference.get_position().get_x() > max_x && reference.get_position().get_y() > max_y) {
        max_x = reference.get_position().get_x();
        max_y = reference.get_position().get_y();
      }

      reference.simulate_step(simulation_interval);
      if (save_last) {
        last_x = reference.get_position().get_x();
      }

      dropping.simulate_step(simulation_interval);
    }

    return (
            test_eq(out,
                    description + " dropping.get_position().get_x",
                    dropping.get_position().get_x(), last_x) +
            test_eq(out,
                    description + " dropping.et_position().get_y",
                    dropping.get_position().get_y(), reference.get_position().get_y()) +
            test_eq(out,
                    description + " dropping.get_max_coordinate().get_x",
                    dropping.get_max_coordinate().get_x(), max_x) +
            test_eq(out,
                    description + " dropping.get_max_coordinate().get_y",
                    dropping.get_max_coordinate().get_y(), max_y) +
            0
            );
  };

  std::vector<int> res = {
    test_projectile(out, 0, 0, 1, 45, 0.001),
    test_projectile(out, 2, 2, 20, 90, 0.001),
    test_projectile(out, 0, 0, 1, 2, 0.001)
  };

#else
  std::vector<int> res = {};
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size() * 4);
}

int projectile_simulation(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_5 == 1
  auto test_projectile = [](ostream& out,
                            const double x, const double y,
                            const double magnitude, const double angle,
                            const double simulation_interval) {
    std::string description = "";
    Projectile reference(Coordinate(x,y), magnitude, angle);
    double last_x = reference.get_position().get_x();
    double max_x = reference.get_position().get_x();
    double max_y = reference.get_position().get_y();

    DroppingProjectile dropping_1(Coordinate(x,y), magnitude, angle);
    Projectile *ptr_proj_1 = &dropping_1;

    description += "DroppingProjectile dropping = DroppingProjectile(Coordinate(" + std::to_string(x) + "," + std::to_string(y) + "),";
    description += std::to_string(angle) + ", " + std::to_string(magnitude) + "); ";
    description += " Projectile *p = &dropping;";
    description += "simulate_full_trajectory(std::cerr, " + std::to_string(simulation_interval) + ", p);}";

    bool save_last = true;
    while (ptr_proj_1->get_position().get_y() >= 0) {
      if (reference.get_velocity_y() > 0) {
        assert(reference.get_position() == ptr_proj_1->get_position());
        assert(reference.get_velocity_x() == ptr_proj_1->get_velocity_x());
        assert(reference.get_velocity_y() == ptr_proj_1->get_velocity_y());
      } else {
        save_last = false;
      }

      if (reference.get_position().get_x() > max_x && reference.get_position().get_y() > max_y) {
        max_x = reference.get_position().get_x();
        max_y = reference.get_position().get_y();
      }

      reference.simulate_step(simulation_interval);
      if (save_last) {
        last_x = reference.get_position().get_x();
      }

      ptr_proj_1->simulate_step(simulation_interval);
    }

    return (
            test_eq(out,
                    description + " p->get_position().get_x",
                    ptr_proj_1->get_position().get_x(), last_x) +
            test_eq(out,
                    description + " p->get_position().get_y",
                    ptr_proj_1->get_position().get_y(), reference.get_position().get_y()) +
            test_eq(out,
                    description + " dropping.get_max_coordinate().get_x",
                    dropping_1.get_max_coordinate().get_x(), max_x) +
            test_eq(out,
                    description + " dropping.get_max_coordinate().get_y",
                    dropping_1.get_max_coordinate().get_y(), max_y) +
            0
            );
  };

  std::vector<int> res = {
    test_projectile(out, 0, 0, 1, 45, 0.001),
    test_projectile(out, 2, 2, 20, 90, 0.001),
    test_projectile(out, 0, 0, 1, 2, 0.001)
  };
#else
  std::vector<int> res = {};
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size() * 4);
}

int projectile_list_node(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_6 == 1
  // Create a node
  //   - get back the element
  //   - next must be null
  auto t1 = [](ostream &out) {
    std::string description = "Projectile(Coordinate(1,2), 1, 45);";
    Projectile p1 = Projectile(Coordinate(1,2), 1, 45);
    ListNode n1 = ListNode(p1);
    return (test_eq(out, description + " n1.get_next", n1.get_next(), (ListNode*) NULL) &&
            test_eq(out, description + " n1.get_projectile().get_position().get_x",
                    n1.get_projectile().get_position().get_x(), p1.get_position().get_x()),
            test_eq(out, description + " n1.get_projectile().get_position().get_y",
                    n1.get_projectile().get_position().get_y(), p1.get_position().get_y()),
            test_eq(out, description + " n1.get_projectile().get_velocity_x",
                    n1.get_projectile().get_velocity_x(), p1.get_velocity_x()),
            test_eq(out, description + " n1.get_projectile().get_velocity_y",
                    n1.get_projectile().get_velocity_y(), p1.get_velocity_y()));
  };

  // Create two nodes
  //   - set a node, get back a node (must be the same)
  auto t2 = [](ostream &out) {
    std::string description = "";
    int success;

    description += "n1 = Projectile(Coordinate(1,2), 1, 45); ";
    description += "n2 = Projectile(Coordinate(2,3), 5, 90); ";
    description += "ListNode *n1 = new ListNode(p1); ";
    description += "ListNode *n2 = new ListNode(p2); ";
    description += "n1->set_next(n2);";

    Projectile p1 = Projectile(Coordinate(1,2), 1, 45);
    Projectile p2 = Projectile(Coordinate(2,3), 5, 90);
    ListNode* n1 = new ListNode(p1);
    ListNode* n2 = new ListNode(p2);
    n1->set_next(n2);

    success = (test_eq(out, description + " n1->get_next", n1->get_next(), n2));

    delete n1;
    delete n2;

    return success;
  };

  std::vector<int> res = {t1(out), t2(out)};
#else
  std::vector<int> res = {};
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}

int projectile_list(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

#if EXERCISE_7 == 1
    // test case for the list
    auto test_list = [](ostream& out, std::vector<Projectile> pvec, const int size_to_process) {
      enum status_type {not_empty_initially=0,
                        empty_after_insertion=1,
                        empty_before_extraction=2,
                        extract_wrong_element=3,
                        not_empty_finally=4} status = not_empty_finally;
      bool success = true;
      List *list = new List();

      auto get_tc_description = [](std::vector<Projectile> pvec, const int size_to_process) {
        std::string description;

        description = description + "List list = new List(); assert(list->is_empty());";
        for(int i = 0; i < size_to_process; ++i) {
          description = description + " list->append(...);";
          description = description + " assert(! list->is_empty());";
        }
        for(int i = 0; i < size_to_process; ++i) {
          description = description + " assert(! list->is_empty());";
          description = description + " list->remove_from_top();";
        }
        description = description + " assert(list->is_empty());";
        return description;
      };

      auto set_success = [](bool &success,
                            status_type &status,
                            const bool new_success,
                            const status_type error_status) {
        if (! new_success) {
          success = false;
          status = error_status;
        }
      };

      auto print_result = [](ostream &out,
                             bool success,
                             status_type status) {
        if (success) {
          out << ": got the right result expected the right result";
        } else {
          switch (status) {
          case(not_empty_initially):
          case(not_empty_finally): {
            out << ": got a list that is not empty" << " expected an empty list";
          }
          break;
          case(empty_after_insertion): {
            out << ": got an empty list after inserting an element" << " expected at least one element";
          }
          break;
          case(empty_before_extraction): {
            out << ": got an empty list before extracting an element" << " expected at least one element";
          }
          break;
          case(extract_wrong_element): {
            out << ": got removed the wrong element" << " expected a different element";
          }
          break;
          default: {
            out << ": got an unknown error" << " expected a different element";
          }
          break;
          }
        }
      };

      std::string description = get_tc_description(pvec, size_to_process);

      set_success(success, status, list->is_empty(), not_empty_initially);
      for (int i=0; i < size_to_process && success; ++i) {
        list->append(pvec[i]);
        set_success(success, status, ! list->is_empty(), empty_after_insertion);
      }

      for (int i=0; i < size_to_process && success; ++i) {
        set_success(success, status, ! list->is_empty(), empty_before_extraction);
        if (success) {
          Projectile app = list->remove_from_top();
          set_success(success, status, app == pvec[i], extract_wrong_element);
        }
      }

      set_success(success, status, list->is_empty(), not_empty_finally);
      delete list;

      out << (success ? "[SUCCESS] " : "[FAILURE] ");
      print_tested_function(out, "", description);
      print_result(out, success, status);
      out << std::endl;

      return success;
    };

    std::vector<Projectile> pvec;
    for (int i = 0; i < 5; ++i) {
      Projectile p(Coordinate(i,i+1),1,45);
      pvec.push_back(p);
    }

    std::vector<int> res = {
      test_list(out, pvec, 0),
      test_list(out, pvec, 1),
      test_list(out, pvec, 2),
      test_list(out, pvec, 5),
    };
#else
    std::vector<int> res = {
    };
#endif

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int projectile_ptr_list(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_8 == 1
  auto t1 = [](ostream& out) {
    std::string description = "";
    double simulation_interval = 0.001;
    PtrList list = PtrList();
    DroppingProjectile p;
    DroppingProjectile p_reference;// p_reference is exactly the same as p

    description += "PtrList list = PtrList(); DroppingProjectile p; list.append(p); Projectile *ptr = list.remove_from_top(); simulate_full_trajectory(out, 0.001, ptr);";

    // Insert p in the list
    list.append(&p);

    // Simulate p_reference
    while (p_reference.get_position().get_y() >= 0) { p_reference.simulate_step(simulation_interval); }

    // Get the point from the list, and simulate it
    Projectile* p_from_list = list.remove_from_top();
    simulate_full_trajectory(std::cerr, simulation_interval, p_from_list);
    //p_from_list->simulate_full_trajectory(simulation_interval);

    return (
            test_eq(out,
                    description + " p->get_position().get_x",
                    p_from_list->get_position().get_x(),
                    p_reference.get_position().get_x()) +
            test_eq(out,
                    description + " p->get_position().get_y",
                    p_from_list->get_position().get_y(),
                    p_reference.get_position().get_y())
            );
  };

  std::vector<int> res = {
    t1(out)
  };

#else
  std::vector<int> res = {};
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size() * 2);
}


int grading(std::ostream &out, const int test_case_number)
{
/**

Annotations used for the autograder.

[START-AUTOGRADER-ANNOTATION]
{
  "total" : 8,
  "names" : ["coordinates_overload_arithmetic",
             "coordinates_overload_comparisons",
             "coordinates_static_fields",
             "projectile_inheritance",
             "projectile_simulation",
             "projectile_list_node",
             "projectile_list",
             "projectile_ptr_list"],
  "points" : [10,5,10,10,20,15,15,15]
}
[END-AUTOGRADER-ANNOTATION]
*/

    int const total_test_cases = 8;
    std::string const test_names[total_test_cases] = {"coordinates_overload_arithmetic",
                                                      "coordinates_overload_comparisons",
                                                      "coordinates_static_fields",
                                                      "projectile_inheritance",
                                                      "projectile_simulation",
                                                      "projectile_list_node",
                                                      "projectile_list",
                                                      "projectile_ptr_list"
    };

    int const points[total_test_cases] = {10,5,10,10,20,15,15,15};
    int (*test_functions[total_test_cases]) (std::ostream &, const std::string) = {
      test_overload_arithmetic,
      test_overload_comparisons,
      coordinates_static_fields,
      projectile_inheritance,
      projectile_simulation,
      projectile_list_node,
      projectile_list,
      projectile_ptr_list
    };

    return run_grading(out, test_case_number, total_test_cases,
                       test_names, points,
                       test_functions);
}

} // End of namepsace tdgrading

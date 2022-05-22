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
#include "common.hpp"
#include "td6.hpp"
#include <limits>

namespace tdgrading {

using namespace testlib;
using namespace std;



int status_target(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_1 == 1
  auto test_aux = [](std::ostream &out, TargetStatus status) {
    Target target;
    target.set_status(status);
    return test_eq(out, "Target target; target.set_status", target.get_status(), status, status);
  };

  std::vector<int> res = {
    test_aux(out, destroyable),
    test_aux(out, unbreakable),
    test_aux(out, hidden),
  };
#else
  std::vector<int> res = {
  };
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}

int change_status(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_2 == 1
  auto test_aux = [](std::ostream &out,
                     TargetStatus src,
                     TargetStatus dst) {
    Target target;
    target.set_status(src);
    target.simulate_step(0.1);
    target.get_status();

    std::string desc = "Target target; target.set_status(" + std::to_string(src) + "); " + "target.simulate_step(0.1); target.get_status";
    

    return test_eq(out, desc, target.get_status(), dst);
  };

  std::vector<int> res = {
    test_aux(out, destroyable, unbreakable),
    test_aux(out, unbreakable, hidden),
    test_aux(out, hidden, destroyable)
  };

#else
  std::vector<int> res = {
  };
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}

int halve_distance(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_3 == 1
  auto test_aux = [](std::ostream &out,
                     Coordinate c_a,
                     Coordinate c_b,
                     Coordinate expected) {
    Coordinate c1(c_a.get_x(), c_a.get_y());
    Coordinate c2(c_b.get_x(), c_b.get_y());
    Coordinate res = halve_distance(c1,c2);

    return test_eq(out, "halve_distance", res, expected, c_a, c_b);
  };

  auto test_aux_const = [](std::ostream &out,
                           Coordinate c_a,
                           Coordinate c_b,
                           Coordinate expected) {
    Coordinate c1(c_a.get_x(), c_a.get_y());
    Coordinate c2(c_b.get_x(), c_b.get_y());
    Coordinate res = halve_distance(c1,c2);

    std::string desc = std::string("halve_distance(") +
    std::string("Coordinate c1(") + std::to_string(c_a.get_x()) + ", " + std::to_string(c_a.get_y()) + ");" +
    std::string("Coordinate c2(") + std::to_string(c_b.get_x()) + ", " + std::to_string(c_b.get_y()) + "));" +
    std::string("c1.get_x");

    return test_eq(out, desc, c1.get_x(), c_a.get_x());
  };


  std::vector<int> res = {
    test_aux(out, Coordinate(), Coordinate(), Coordinate()),
    test_aux(out, Coordinate(1,1), Coordinate(1,1), Coordinate(1,1)),
    test_aux(out, Coordinate(5,1), Coordinate(3,1), Coordinate(4,1)),
    test_aux(out, Coordinate(3,1), Coordinate(5,1), Coordinate(4,1)),
    test_aux(out, Coordinate(1,5), Coordinate(1,3), Coordinate(1,4)),
    test_aux(out, Coordinate(1,3), Coordinate(1,5), Coordinate(1,4)),
    test_aux(out, Coordinate(5,3), Coordinate(3,7), Coordinate(4,5)),
    //
    test_aux_const(out, Coordinate(5,3), Coordinate(3,7), Coordinate(4,5)),
    test_aux_const(out, Coordinate(3,1), Coordinate(5,1), Coordinate(4,1)),
  };
#else
  std::vector<int> res = {
  };
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}

#if EXERCISE_4 == 1
int test_count_segments(std::ostream &out,
                      Coordinate c_a,
                      Coordinate c_b,
                      double min_distance,
                      int expected,
                      bool count_instantiations) {
  Coordinate c1(c_a.get_x(), c_a.get_y());
  Coordinate c2(c_b.get_x(), c_b.get_y());
  Coordinate res = halve_distance(c1,c2);

  if (not count_instantiations) {
    return test_eq(out, "count_half_segments",
                   count_half_segments(c1,c2,min_distance),
                   expected, c_a, c_b, min_distance);
  } else {
    return test_eq(out, "count_coordinate_instances",
                   count_coordinate_instances(c1, c2, min_distance),
                   expected,
                   c_a, c_b, min_distance);
  }
};
#endif

int count_segments(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_4 == 1

  auto test_aux = [](std::ostream &out,
                     Coordinate c_a,
                     Coordinate c_b,
                     double min_distance,
                     int expected) {
    return test_count_segments(out,c_a,c_b,min_distance,expected,false);
  };


  std::vector<int> res = {
    test_aux(out, Coordinate(), Coordinate(), 1.0, 0),
    test_aux(out, Coordinate(), Coordinate(1,1), 1.0, 0),
    test_aux(out, Coordinate(), Coordinate(2,0), 1, 1),
    test_aux(out, Coordinate(), Coordinate(4,0), 1, 3),
    test_aux(out, Coordinate(), Coordinate(5,0), 1, 3),
    //
    test_aux(out, Coordinate(), Coordinate(1,1), 1.0, 0),
    test_aux(out, Coordinate(), Coordinate(0,2), 1, 1),
    test_aux(out, Coordinate(), Coordinate(0,4), 1, 3),
    test_aux(out, Coordinate(), Coordinate(0,5), 1, 3),
    //
    test_aux(out, Coordinate(1,1), Coordinate(2,2), 1.0, 0),
    test_aux(out, Coordinate(1,1), Coordinate(2,2), 0.5, 1)
  };
#else
  std::vector<int> res = {
  };
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}

int count_coordinate_instances(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_5 == 1
  auto test_aux = [](std::ostream &out,
                     Coordinate c_a,
                     Coordinate c_b,
                     double min_distance,
                     int expected) {
    return test_count_segments(out,c_a,c_b,min_distance,expected,true);
  };

  std::vector<int> res = {
    test_aux(out, Coordinate(), Coordinate(), 1.0, 1),
    test_aux(out, Coordinate(), Coordinate(2,0), 1, 3),
    test_aux(out, Coordinate(), Coordinate(0,5), 1, 7),
    test_aux(out, Coordinate(), Coordinate(0,10), 1, 15),
    test_aux(out, Coordinate(), Coordinate(0,20), 1, 31)
  };
#else
  std::vector<int> res = {
  };
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}

int get_distance_template(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_6 == 1
  std::vector<int> res = {
    test_eq_approx(out, "get_distance", get_distance(Target(),Target()), 0.0, 0.01, Target(), Target()),
    test_eq_approx(out, "get_distance", get_distance(Target(Coordinate(2,3),2.0),Target()), 3.6, 0.01,
                   Target(Coordinate(2,3),2.0),Target()),
    test_eq_approx(out, "get_distance", get_distance(Projectile(),Projectile()), 0.0, 0.01, Projectile(), Projectile()),
    test_eq_approx(out, "get_distance", get_distance(Projectile(),Target()), 0.0, 0.01, Projectile(), Target()),
  };
#else
  std::vector<int> res = {
  };
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}

int list_templates(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_7 == 1
  auto t1 = [](std::ostream &out) {
    ProjectileList *l;
    Projectile res;
    init_list(l);
    std::string desc = "ProjectileList *l; init_list(l); is_list_empty";

    return test_eq(out, desc, is_list_empty(l), true, "l");
  };

  auto t1b = [](std::ostream &out) {
    TargetList *l;
    Target res;
    init_list(l);
    std::string desc = "TargetList *l; init_list(l); is_list_empty";

    return test_eq(out, desc, is_list_empty(l), true, "l");
  };

  auto t2 = [](std::ostream &out) {
    ProjectileList *l;
    Projectile p1(Coordinate(1,1),1,45);
    Projectile res;
    init_list(l);
    append(l, p1);
    remove_from_head(l,res);

    std::string descr = "ProjectileList *l; Projectile p1(Coordinate(1,1),1,45); Projectile res; init_list(l); append(l, p1); remove_from_head(l,res); ";
    return test_eq(out, descr, res, p1, "res");
  };

  auto t2b = [](std::ostream &out) {
    TargetList *l;
    Target t1(Coordinate(1,1),1);
    Target res;
    init_list(l);
    append(l, t1);
    remove_from_head(l,res);

    std::string descr = "TargetList *l; Target t1(Coordinate(1,1),1); Target res; init_list(l); append(l, t1); remove_from_head(l,res); ";
    return test_eq(out, descr, res, t1, "res");
  };

  auto t3 = [](std::ostream &out) {
    ProjectileList *l;
    Projectile p1(Coordinate(1,1),1,45);
    Projectile res;
    init_list(l);
    append(l, p1);
    remove_from_tail(l,res);
    std::string desc = "ProjectileList *l; Projectile p1(Coordinate(1,1),1,45); Projectile res; init_list(l); append(l, p1); remove_from_tail(l,res); ";
    // delete l;
    return test_eq(out, desc, res, p1, "res");
  };

  auto t3b = [](std::ostream &out) {
    TargetList *l;
    Target t1(Coordinate(1,1),1);
    Target res;
    init_list(l);
    append(l, t1);
    remove_from_tail(l,res);
    std::string desc = "TargetList *l; Target t1(Coordinate(1,1),1); Target res; init_list(l); append(l, t1); remove_from_tail(l,res); ";
    // delete l;
    return test_eq(out, desc, res, t1, "res");
  };

  std::vector<int> res = {
    t1(out),
    t1b(out),
    t2(out),
    t2b(out),
    t3(out),
    t3b(out),
  };
#else
  std::vector<int> res = {
  };
#endif

  return end_test_suite(out, test_name,
                        accumulate(res.begin(), res.end(), 0), res.size());
}


int list_copy(std::ostream &out, const std::string test_name) {
  start_test_suite(out, test_name);

#if EXERCISE_8 == 1

  auto t1 = [](std::ostream &out) {
    ProjectileList *l1, *l2;
    init_list(l1);
    l2 = NULL;
    copy(l1,l2, Projectile());

    std::string desc = "ProjectileList *l1, *l2; init_list(l1); copy(l1, l2, Projectile()); l2->is_empty";

    // do not delete (memleak but ok)
    return test_eq(out, desc, l2->is_empty(), l1->is_empty());
  };

  auto t1b = [](std::ostream &out) {
    TargetList *l1, *l2;
    init_list(l1);
    l2 = NULL;
    copy(l1,l2, Target());

    std::string desc = "TargetList *l1, *l2; init_list(l1); copy(l1, l2, Target()); l2->is_empty";

    // do not delete (memleak but ok)
    return test_eq(out, desc, l2->is_empty(), l1->is_empty());
  };

  auto t2 = [](std::ostream &out) {
    ProjectileList *l1, *l2;
    Projectile p1(Coordinate(1,1),1,45);
    Projectile p2(Coordinate(2,2),1,45);
    init_list(l1);
    append(l1, p1);
    append(l1, p2);
    l2 = NULL;
    copy(l1,l2, Projectile(Coordinate(4,5),1,45));
    Projectile res;
    remove_from_head(l2, res);
    std::string desc = "ProjectileList *l1, *l2; Projectile p1(Coordinate(1,1),1,45); Projectile p2(Coordinate(2,2),1,45); init_list(l1); append(l1, p1); append(l1, p2); copy(l1,l2, Projectile(Coordinate(4,5),1,45)); Projectile res; remove_from_head(l2,res); ";
    return test_eq(out, desc, res, p1, "res");
  };

  auto t3 = [](std::ostream &out) {
    ProjectileList *l1, *l2;
    Projectile res;
    Projectile p1(Coordinate(1,1),1,45);
    Projectile p2(Coordinate(2,2),1,45);
    init_list(l1);
    append(l1, p1);
    append(l1, p2);
    l2 = NULL;
    copy(l1,l2, Projectile(Coordinate(4,5),1,45));
    remove_from_tail(l2,res);
    std::string desc = "ProjectileList *l1, *l2; Projectile p1(Coordinate(1,1),1,45); Projectile p2(Coordinate(2,2),1,45); init_list(l1); append(l1, p1); append(l1, p2); copy(l1,l2, Projectile(Coordinate(4,5),1,45)); Projectile res; remove_from_tail(l2,res); ";
    return test_eq(out, desc, res, p2, "res");
  };

  auto t4 = [](std::ostream &out) {
    ProjectileList *l1, *l2;
    Projectile p1(Coordinate(1,1),1,45);
    Projectile p2(Coordinate(2,2),1,45);
    init_list(l1);
    append(l1, p1);
    append(l1, p2);
    l2 = NULL;
    copy(l1,l2, p2);
    Projectile res;
    remove_from_tail(l2, res);
    std::string desc = "ProjectileList *l1, *l2; Projectile p1(Coordinate(1,1),1,45); Projectile p2(Coordinate(2,2),1,45); init_list(l1); append(l1, p1); append(l1, p2); copy(l1,l2, p1); Projectile res; remove_from_tail(l2,res); ";
    return test_eq(out, desc, res, p1, "res");
  };

  auto t5 = [](std::ostream &out) {
    ProjectileList *l1, *l2;
    Projectile p1(Coordinate(1,1),1,45);
    Projectile p2(Coordinate(2,2),1,45);
    init_list(l1);
    append(l1, p1);
    append(l1, p2);
    l2 = NULL;
    copy(l1,l2, p1);
    Projectile res;
    remove_from_tail(l2, res);
    std::string desc = "ProjectileList *l1, *l2; Projectile p1(Coordinate(1,1),1,45); Projectile p2(Coordinate(2,2),1,45); init_list(l1); append(l1, p1); append(l1, p2); copy(l1,l2, p1); Projectile res; remove_from_tail(l2,res); ";
    return test_eq(out, desc, res, p2, "res");
  };


  std::vector<int> res = {
    t1(out),
    t1b(out),
    t2(out),
    t3(out),
    t4(out),
    t5(out),
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
  "total" : 8,
  "names" : [
     "status_target",
      "change_status",
      "halve_distance",
      "count_segments",
      "count_coordinate_instances",
      "get_distance_template",
      "list_templates",
      "list_copy"
  ],
  "points" : [5,5,10,10,10,20,25,15]
}
[END-AUTOGRADER-ANNOTATION]
*/

    int const total_test_cases = 8;
    std::string const test_names[total_test_cases] = {
      "status_target",
      "change_status",
      "halve_distance",
      "count_segments",
      "count_coordinate_instances",
      "get_distance_template",
      "list_templates",
      "list_copy",
    };
    int const points[total_test_cases] = {5,5,10,10,10,20,25,15};
    int (*test_functions[total_test_cases]) (std::ostream &, const std::string) = {
      status_target,
      change_status,
      halve_distance,
      count_segments,
      count_coordinate_instances,
      get_distance_template,
      list_templates,
      list_copy
    };

    return run_grading(out, test_case_number, total_test_cases,
                       test_names, points,
                       test_functions);
}

} // End of namepsace tdgrading

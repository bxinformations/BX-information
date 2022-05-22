#include <iostream>
#include <iomanip>
#include <sstream>
#include <cstdarg>
#include <iterator>
#include <string>
#include <regex>
#include <numeric>
#include <cmath>
#include <limits>

#include <tuple>
#include <vector>

#include "../gradinglib/gradinglib.hpp"

#include "td4.hpp"


namespace tdgrading {

using namespace testlib;
using namespace std;



int test_coordinate(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

    // test cases
    // - initialize to 0,0
    // - initialize with standard value
    // - get_x, get_y
    // - test print coordinates

    std::vector<int> res = {
#if EXERCISE_1 == 1
      test_eq(out, "Coordinate().get_x", Coordinate().get_x(), 0),
      test_eq(out, "Coordinate().get_y", Coordinate().get_y(), 0),
      test_eq(out, "Coordinate(2,3).get_x", Coordinate(2,3).get_x(), 2),
      test_eq(out, "Coordinate(2,3).get_x", Coordinate(2,3).get_y(), 3),
      //
      test_eq(out, "Coordinate().get_distance",
              Coordinate().get_distance(Coordinate()), 0, "Coordinate()"),
      test_eq_approx(out, "Coordinate(1,2).get_distance",
                     Coordinate(1,2).get_distance(Coordinate()), 2.23606797749978969640, 0.0001, "Coordinate()"),
      test_eq(out, "Coordinate(1,2).get_distance",
              Coordinate(1,2).get_distance(Coordinate(3,10)), 8.24621125123532109964, 0.0001, "Coordinate(3,10)"),
#endif
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int test_target(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

#if EXERCISE_2 == 1
    auto test_target_random = [](std::ostream &out)
      {
        Target target = Target(Coordinate(-1,-1), 1.0);
        target.randomize();
        return test_eq_range(out, "target.randomize(); target.get_position().get_x",
                             target.get_position().get_x(),
                             0.0, 100.0);
      };
#endif

    std::vector<int> res = {
#if EXERCISE_2 == 1
      test_eq(out, "Target(Coordinate(),1.0).get_position().get_x", Target(Coordinate(),1.0).get_position().get_x(), 0),
      test_eq(out, "Target(Coordinate(),1.0).get_position().get_y", Target(Coordinate(),1.0).get_position().get_y(), 0),
      test_eq(out, "Target(Coordinate(2,3),1.0).get_position().get_x", Target(Coordinate(2,3),1.0).get_position().get_x(), 2),
      test_eq(out, "Target(Coordinate(2,3),1.0).get_position().get_x", Target(Coordinate(2,3),1.0).get_position().get_y(), 3),
      //
      test_eq(out, "Target().get_position().get_x", Target().get_position().get_x(), 0),
      test_eq(out, "Target().get_position().get_y", Target().get_position().get_y(), 0),
      test_eq(out, "Target().get_radius()", Target().get_radius(), 1.0),
      //
      test_eq(out, "Target().get_radius", Target().get_radius(), 1.0),
      //
      test_eq(out, "Target(Coordinate(2,3),2.0).get_radius", Target(Coordinate(2,3),2.0).get_radius(), 2),
      //
      test_target_random(out)
#endif
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}



int test_projectile(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

    // test cases
    // - Create default
    // - Create with some values

    std::vector<int> res = {
#if EXERCISE_3 == 1
      test_eq(out, " Projectile().get_position().get_x", Projectile().get_position().get_x(), 0, ""),
      test_eq(out, "Projectile().get_position().get_y", Projectile().get_position().get_y(), 0, ""),
      test_eq_approx(out, "Projectile().get_velocity_x", Projectile().get_velocity_x(), 0.707107, 0.0001, ""),
      test_eq_approx(out, "Projectile().get_velocity_y", Projectile().get_velocity_y(), 0.707107, 0.0001, ""),
      //
      test_eq(out, "Projectile(Coordinate(),10,70).get_position().get_x", Projectile(Coordinate(),10,70).get_position().get_x(), 0, ""),
      test_eq(out, "Projectile(Coordinate(),10,70).get_position().get_y", Projectile(Coordinate(),10,70).get_position().get_y(), 0, ""),
      test_eq_approx(out, "Projectile(Coordinate(),10,70).get_velocity_x", Projectile(Coordinate(),10,70).get_velocity_x(), 3.4202, 0.0001, ""),
      test_eq_approx(out, "Projectile(Coordinate(),10,70).get_velocity_y", Projectile(Coordinate(),10,70).get_velocity_y(), 9.39693, 0.0001, "")
#endif
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}


#if EXERCISE_4 == 1
bool test_simulate_step_aux(std::ostream &out,
                            const double pos_x, const double pos_y,
                            const double magnitude, const double angle, const double delta,
                            const double next_x, const double next_y) {
  bool success;
  double x,y;
  Projectile p(Coordinate(pos_x,pos_y), magnitude, angle);
  std::string creation = "Projectile p(Coordinate(" +
    std::to_string(pos_x) + ", " + std::to_string(pos_y) + "), " +
    std::to_string(magnitude) + ", " +
    std::to_string(angle) + ")";

  p.simulate_step(delta);
  x = p.get_position().get_x();
  y = p.get_position().get_y();

  success = (abs(x - next_x) <= 0.0001 && abs(y - next_y) <= 0.0001);

  out << (success ? "[SUCCESS] " : "[FAILURE] ");
  print_tested_function(out, creation + ";p.simulate", delta);

  out.precision(5);
  out << ": got " << "x=" << x << " and y=" << y
      << " expected " << "x=" << next_x << " and y=" << next_y;
  out << std::endl;

  return success;
}
#endif

int test_simulate(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

    // test simulate step

    std::vector<int> res = {
#if EXERCISE_4 == 1
      //
      test_simulate_step_aux(out, 0, 0, 0, 0, 0.05, 0, -0.03675),
      test_simulate_step_aux(out, 5, 6, 45, 70, 0.001, 5.0154, 6.0423),
      test_simulate_step_aux(out, 3, 3, 45, 70, 0.001, 3.0154, 3.0423),
      test_simulate_step_aux(out, 10, 10, 50, 20, 0.001, 10.047, 10.017)
#endif
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

#if EXERCISE_5 == 1
bool test_intersect_aux(std::ostream &out,
                        const double x1, const double y1,
                        const double x2, const double y2, double radius,
                        const bool expected) {
  bool res, success;
  std::string creation = "Projectile p(Coordinate(" +
    std::to_string(x1) + ", " + std::to_string(y1) + "0, 0); " +
    "Target t(Coordinate(" + std::to_string(x2) + ", " + std::to_string(y2) + ")," +
    std::to_string(radius) + ")";

  Projectile p(Coordinate(x1, y1), 0, 0);
  Target t(Coordinate(x2,y2), radius);

  res = p.intersect(t);

  success = (res == expected);

  out << (success ? "[SUCCESS] " : "[FAILURE] ");
  print_tested_function(out, creation + ";p.intersect", "t");

  out << ": got " << res
      << " expected " << expected;
  out << std::endl;

  return success;

}
#endif

int test_intersect(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

    std::vector<int> res = {
#if EXERCISE_5 == 1
      test_intersect_aux(out, 0,0,0,0,1.0,true),
      test_intersect_aux(out, 5.0,0.0,10,0,1.0,false),
      test_intersect_aux(out, 5.0,0.0,5.5,0.5,1.0,true),
      test_intersect_aux(out, 5.0,0.0,10,0,1.0,false),
      test_intersect_aux(out, 20.0,0.0,19.8,0.1,1.0,true),
      test_intersect_aux(out, 20.0,0.0,9.8,0.1,1.0,false),
      //
      test_intersect_aux(out, 0,0,0,0,2.0,true),
      test_intersect_aux(out, 5.0,0.0,5.5,0.5,2.0,true),
      test_intersect_aux(out, 20.0,0.0,19.8,0.1,2.0,true),
      //
      test_intersect_aux(out, 5.0,0.0,10,0,1.0,false),
      test_intersect_aux(out, 5.0,0.0,10,0,1.0,false),
      test_intersect_aux(out, 20.0,0.0,9.8,0.1,1.0,false),
      //
      test_intersect_aux(out, 5.0,0.0,7,0.0,2.0,true),
      test_intersect_aux(out, 5.0,0.0,7.1,0.0,2.0,false)
#endif
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

#if EXERCISE_6 == 1
void fill_telemetry(Telemetry &telemetry,
                    const double x_0, const double y_0,
                    const double vx_0, const double vy_0,
                    const double simulation_interval,
                    const double tx, const double ty) {
  double x, y, t, vx, vy;
  bool hit_target;
  double g = -9.8;

  t = 0;
  x = x_0;
  y = y_0;
  vx = vx_0;
  vy = vy_0;

  hit_target = hypot(x - tx, y - ty) <= 1.0;
  while (y >= 0 && (! hit_target)) {
    telemetry.add_point(t,x,y);

    t = t + simulation_interval;
    vy  = vy + g * simulation_interval;
    y = y + vy * simulation_interval + 0.5 * g * simulation_interval * simulation_interval;
    x = x + vx * simulation_interval;

    if (hypot(x - tx, y - ty) <= 1.0) {
      hit_target = true;
    }
  }
}
#endif

int test_telemetry(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

#if EXERCISE_6 == 1
    auto add1 = [](std::ostream &out)
      {
        Telemetry t;
        t.add_point(0,0,0);
        return test_eq(out, "Telemetry t; t.add_point(0,0,0); t.get_tot_point",
                       t.get_tot_points(), 1);
      };

    auto add2 = [](std::ostream &out)
      {
        Telemetry t;
        t.add_point(0,0,0);
        t.add_point(1,2,3);
        return test_eq(out, "Telemetry t; t.add_point(0,0,0); t.add_point(1,2,3); t.get_tot_point",
                       t.get_tot_points(), 2);
      };

    auto addn = [](std::ostream &out, const int n)
      {
        Telemetry t;
        for (int i = 0; i < n; i++) {
          t.add_point(i,i,i);
        }

        return test_eq(out, "Telemetry t; < here we add " + std::to_string(n) + " points >; t.get_tot_point",
                       t.get_tot_points(), n);
      };


    auto compare_x = [](std::ostream &out,
                        std::string code,
                        double tres,
                        double xres,
                        double yres,
                        double texp,
                        double xexp,
                        double yexp) {
      bool success = (tres == texp && xres == xexp && yres == yexp);
      out << (success ? testlib::get_success() : testlib::get_failure()) << " ";
      out << code;
      out << ": got ";
      out << "time = " << tres << ",x = " << xres << ",y = " << yres;
      out << " expected ";
      out << "time = " << texp << ",x = " << xexp << ",y = " << yexp;
      out << std::endl;
      return success;
    };

    auto get0 = [](std::ostream &out, bool (*compare_x)(std::ostream&,std::string,double,double,double,double,double,double))
      {
        Telemetry t;
        double time,x,y;
        t.add_point(0,0,0);
        t.add_point(1,2,3);
        t.get_point(0, time, x, y);
        return compare_x(out,
                         "Telemetry t; t.add_point(0,0,0); t.add_point(1,2,3); t.get_tot_point(time,x,y);",
                         time, x, y,
                         0,0,0);
      };

    auto get1 = [](std::ostream &out, bool (*compare_x)(std::ostream&,std::string,double,double,double,double,double,double))
      {
        Telemetry t;
        double time,x,y;
        t.add_point(0,0,0);
        t.add_point(1,2,3);
        t.get_point(1, time, x, y);
        return compare_x(out,
                         "Telemetry t; t.add_point(0,0,0); t.add_point(1,2,3); t.get_tot_point(time,x,y);",
                         time, x, y,
                         1,2,3);
      };
#endif

    std::vector<int> res = {
#if EXERCISE_6 == 1
      test_eq(out, "Telemetry().get_tot_points", Telemetry().get_tot_points(), 0),
      add1(out),
      add2(out),
      addn(out,5),
      addn(out,50),
      get0(out, compare_x),
      get1(out, compare_x),
#endif
    };

    return end_test_suite(out, test_name,
                          accumulate(res.begin(), res.end(), 0), res.size());
}

int test_game(std::ostream &out, const std::string test_name) {
    start_test_suite(out, test_name);

#if EXERCISE_7 == 1
    auto test_game_aux = [](std::ostream &out,
                            double x, double y, double magnitude, double angle,
                            double xt, double yt)
      {
        bool success = true;
        int res_count = 0;
        int exp_count = 0;
        double simulation_interval = 0.01;
        std::tuple<double,double,double> exp_tuple;
        std::tuple<double,double,double> res_tuple;

        Telemetry expected_telemetry = Telemetry();

        Projectile p = Projectile(Coordinate(x,y), magnitude, angle);
        Target t = Target(Coordinate(xt,yt),1.0);

        Game g(p, t);
        g.run(simulation_interval);

        fill_telemetry(expected_telemetry,
                       p.get_position().get_x(),
                       p.get_position().get_y(),
                       p.get_velocity_x(),
                       p.get_velocity_y(),
                       simulation_interval,
                       t.get_position().get_x(),
                       t.get_position().get_y());

        res_count = g.telemetry.get_tot_points();
        exp_count = expected_telemetry.get_tot_points();
        if (res_count == exp_count) {
          for (int i = 0; i < res_count; ++i) {
            double t,x,y,t1,x1,y1;

            g.telemetry.get_point(i,t,x,y);
            expected_telemetry.get_point(i,t1,x1,y1);

            if ( t != t1 || x != x1 || y != y1) {
              res_tuple = std::make_tuple(t,x,y);
              exp_tuple = std::make_tuple(t1,x1,y1);
              success = false;
            }

          }
        } else {
          success = false;
        }

        out << (success ? testlib::get_success() : testlib::get_failure()) << " ";

        out << "Projectile(Coordinate(" << x << "," << y << ")," << magnitude << "," << angle << "); ";
        out << "Target(Coordinate(" << x << "," << y << ")" << ",1.0); ";
        out << "Game g(p, t); game.run(" << simulation_interval << ");";
        out << ": got ";

        if (res_count != exp_count || success) {
          out << res_count << " elements";
        } else {
          out << " the element (" << get<0>(res_tuple) << ",";
          out << get<1>(res_tuple) << ",";
          out << get<2>(res_tuple) << ")";
        }

        out << " expected ";

        if (res_count != exp_count || success) {
          out << exp_count << " elements";
        } else {
          out << " the element (" << get<0>(exp_tuple) << ",";
          out << get<1>(exp_tuple) << ",";
          out << get<2>(exp_tuple) << ")";
        }

        out << std::endl;
        return success;
      };
#endif

    std::vector<int> res = {
#if EXERCISE_7 == 1
      test_game_aux(out,0,0,40,60,80,60),
      test_game_aux(out,0,0,35,60,80,60),
      test_game_aux(out,0,0,20,70,80,60),
      //
      test_game_aux(out,0,0,40,60,80,36),
      test_game_aux(out,0,0,35,60,80,36),
      test_game_aux(out,0,0,20,70,80,36)
#endif
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
  "total" : 7,
  "names" : ["test_coordinate",
      "test_target",
      "test_projectile",
      "test_simulate",
      "test_intersect",
      "test_telemetry",
      "test_game"],
  "points" : [5,10,15,10,5,30,25]
}
[END-AUTOGRADER-ANNOTATION]
*/

    int const total_test_cases = 7;
    std::string const test_names[total_test_cases] = {
      "test_coordinate",
      "test_target",
      "test_projectile",
      "test_simulate",
      "test_intersect",
      "test_telemetry",
      "test_game"
    };

    int const points[total_test_cases] = {5,10,15,10,5,30,25};
    int (*test_functions[total_test_cases]) (std::ostream &, const std::string) = {
      test_coordinate,
      test_target,
      test_projectile,
      test_simulate,
      test_intersect,
      test_telemetry,
      test_game
    };

    /* initialize random seed: */
    srand(0);

    return run_grading(out, test_case_number, total_test_cases,
                       test_names, points,
                       test_functions);
}

} // End of namepsace tdgrading

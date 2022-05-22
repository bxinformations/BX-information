/**
  * CSE201 - TD 5
  *
  * Work on your implementation in the td5.cpp file.
  *
  * You should not change any other file, with the exception of main.cpp
  * if you wish to debug your code.
  *
  * The main function in main.cpp runs the automatic grader by default (this is
  * what you need to run while writing your solutions).
  *
  * The value of the macro GRADING defines what code is executed in the main function.
  * If the value is 1 the program runs the automatic grading of the assignment.
  *
  * If the value is 0 (or more precisely, different from 1) the program runs the
  * code in the custom code section below (between the #if GRADING != 1 and
  * #else directives).
  *
  * You can always run the automatic grader just changing the definition of
  * GRADING to 0.
  */

#include <iostream>
#include "grading/grading.hpp"

#include "td5.hpp"

using namespace std;

#define GRADING 1

int main(int argc, char* argv[])
{
#if GRADING != 1
    // START OF THE CUSTOM CODE SECTION
    // This code will be executed only if you set GRADING to a value different from 1

#if EXERCISE_4 == 1
    double simulation_interval = 0.001;
    DroppingProjectile p1,p2,p3;
    // p1_references is equal to p1, p2_references is equal to p2, and p3_references is equal to p3
    DroppingProjectile p1_reference, p2_reference, p3_reference;

    // Show the implementation issue for exercise 4
    {
      simulate_full_trajectory(std::cerr, simulation_interval, p3_reference); // this function takes a DroppingProjectile as input - so no problem here
      simulate_full_trajectory(std::cerr, simulation_interval, &p3);

      // Why are the coordinate of p1_reference and p1 different?
      std::cout << "Demonstrates the implementation issue for exercise 4:" << std::endl;
      std::cout << "Expected result: ("
                << p3_reference.get_position().get_x() << ","
                << p3_reference.get_position().get_y() << ")" << std::endl;

      std::cout << "Position of p1: ("
                << p3.get_position().get_x() << ","
                << p3.get_position().get_y() << ")\n" << std::endl;
    }
#endif

#if EXERCISE_7 == 1
    // Show the implementation that motivates exercise 8
    {
      List list = List();

      // Insert p in the list
      list.append(p2);

      // Simulate p_reference
      simulate_full_trajectory(std::cerr, simulation_interval, &p2_reference);

      // Get the point from the list, and simulate it
      Projectile p_from_list = list.remove_from_top();
      simulate_full_trajectory(std::cerr, simulation_interval, &p_from_list);

      // Why are the coordinate of p2_reference and p_from_list different?
      std::cout << "Demonstrates the implementation issue for exercise 6 and 7:" << std::endl;
      std::cout << "Expected result: ("
                << p2_reference.get_position().get_x() << ","
                << p2_reference.get_position().get_y() << ")" << std::endl;

      std::cout << "Position of point from List ("
                << p_from_list.get_position().get_x() << ","
                << p_from_list.get_position().get_y() << ")\n" << std::endl;
    }
#endif

#if EXERCISE_8 == 1
    // Shows that exercise 8 works for subtypes of `Projectiles`
    {
      PtrList list = PtrList();

      // Insert p in the list
      list.append(&p3);

      // Simulate p_reference
      simulate_full_trajectory(std::cerr, simulation_interval, &p3_reference);

      // Get the point from the list, and simulate it
      Projectile *ptr_from_list = list.remove_from_top();
      simulate_full_trajectory(std::cerr, simulation_interval, ptr_from_list);

      // Why are the coordinate of p3_reference and ptr_from_list different?
      std::cout << "Demonstrates the use of pointers instead of copying the objects:" << std::endl;
      std::cout << "Expected result: ("
                << p2_reference.get_position().get_x() << ","
                << p2_reference.get_position().get_y() << ")" << std::endl;

      std::cout << "Position of point from PtrList ("
                << ptr_from_list->get_position().get_x() << ","
                << ptr_from_list->get_position().get_y() << ")" << std::endl;
    }
#endif

    // END OF THE CUSTOM CODE SECTION
#else
    // RUN THE AUTOMATIC GRADER
    {
      int test_number = 0; // run all the tests

      if (argc == 2) {
        test_number = stoi(argv[1]);
      }

      return tdgrading::grading(std::cerr, test_number);
      // END OF THE AUTOMATIC GRADER
    }
#endif
}

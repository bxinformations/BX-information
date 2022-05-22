/**
  * CSE201 - TD 2
  *
  * Work on your implementation in the td2.cpp file.
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
#include <stdlib.h>     // include rand
#include <time.h>       // include time
#include "grading/grading.hpp"

#include "td2.hpp"

using namespace std;

#define GRADING 1

int main(int argc, char* argv[])
{
#if GRADING != 1
    // START OF THE CUSTOM CODE SECTION
    // This code will be executed only if you set GRADING to a value different from 1

    /* initialize random seed: */
    srand (time(NULL));

    run_game(std::cout, std::cin);

    // END OF THE CUSTOM CODE SECTION
#else
    // RUN THE AUTOMATIC GRADER
    {
      int test_number = 0; // run all the tests

      /* initialize random seed: */
      srand(0);

      if (argc == 2) {
        test_number = stoi(argv[1]);
      }

      return tdgrading::grading(std::cerr, test_number);
      // END OF THE AUTOMATIC GRADER
    }
#endif
}

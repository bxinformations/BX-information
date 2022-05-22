/**
  * CSE201 - TD 1
  *
  * Work on your implementation in the td1.cpp file.
  *
  * You should not change any other file, with the exception of main.cpp
  * if you wish to play the final game or if you need to debug your code.
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
  * Now that code runs the function "play_game".
  *
  * You may want to run your play_game or some different code code to test and
  * debug your implementation, instead of the automatic grader.
  * You achieve this by setting the GRADING macro to 0.
  *
  * You can always run the automatic grader just changing the definition of
  * GRADING to 0.
  */

#include <iostream>

#include "td1.hpp"
#include "grading/grading.hpp"

using namespace std;

#define GRADING 1

int main(int argc, char* argv[])
{
#if GRADING != 1
    // START OF THE CUSTOM CODE SECTION
    // This code will be executed only if you set GRADING to a value ddifferent from 1
    return play_game(std::cout, std::cin);
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

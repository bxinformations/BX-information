#ifndef TEST_HPP
#define TEST_HPP

#include <iostream>
#include <iomanip>
#include <sstream>
#include <cstdarg>
#include <iterator>
#include <string>
#include <regex>
#include <numeric>
#include <cmath>

#include <cmath>
#include "td7.hpp"
#include "../gradinglib/gradinglib.hpp"

namespace tdgrading {
int grading(std::ostream &out, const int test_case);

template <typename T>
std::string get_tc_description(std::string tname, std::vector<T> pvec, const int size_to_process) {
  std::string description;

  description = description + "List<" + tname + "> list = new List<" + tname + ">(); assert(list->is_empty());";
  for(int i = 0; i < size_to_process; ++i) {
    std::stringstream ss;
    ss << pvec[i];

    description = description + " list->append(" + ss.str() + ");";
    description = description + " assert(! list->is_empty());";
  }
  for(int i = 0; i < size_to_process; ++i) {
    description = description + " assert(! list->is_empty());";
    description = description + " list->remove_from_top();";
  }
  description = description + " assert(list->is_empty());";
  return description;
}

#if EXERCISE_3 == 1
template <typename T>
int test_list(std::ostream& out, std::string tname, std::vector<T> evec, const int size_to_process) {
  enum status_type {not_empty_initially=0,
                    empty_after_insertion=1,
                    empty_before_extraction=2,
                    extract_wrong_element=3,
                    not_empty_finally=4} status = not_empty_finally;
  bool success = true;
  List<T> *list = new List<T>();

  auto set_success = [](bool &success,
                        status_type &status,
                        const bool new_success,
                        const status_type error_status) {
    if (! new_success) {
      success = false;
      status = error_status;
    }
  };

  auto print_result = [](std::ostream &out,
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

  std::string description = get_tc_description(tname, evec, size_to_process);

  set_success(success, status, list->is_empty(), not_empty_initially);
  for (int i=0; i < size_to_process && success; ++i) {
    list->append(evec[i]);
    set_success(success, status, ! list->is_empty(), empty_after_insertion);
  }

  for (int i=0; i < size_to_process && success; ++i) {
    set_success(success, status, ! list->is_empty(), empty_before_extraction);
    if (success) {
      T app = list->remove_from_head();
      set_success(success, status, app == evec[i], extract_wrong_element);
    }
  }

  set_success(success, status, list->is_empty(), not_empty_finally);
  delete list;

  out << (success ? "[SUCCESS] " : "[FAILURE] ");
  testlib::print_tested_function(out, "", description);
  print_result(out, success, status);
  out << std::endl;

  return success;
};
#endif

}

#endif // TEST_HPP

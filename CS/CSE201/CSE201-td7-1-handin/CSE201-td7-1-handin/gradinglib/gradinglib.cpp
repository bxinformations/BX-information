#include "gradinglib.hpp"

#include <iostream>
#include <iomanip>
#include <cstdarg>
#include <iterator>
#include <string>
#include <regex>
#include <numeric>
#include <cmath>

namespace testlib {

  using namespace std;

  bool match(const string &str, const string regexp) {
    auto const re = regex{regexp};
    return regex_search(str, re);
  }

  std::string get_success() {
    return "[SUCCESS]";
  }

  std::string get_failure() {
    return "[FAILURE]";
  }

  void print(std::ostream &out) {
    out << "";
  }


  void start_test_suite(ostream &out, const string &name) {
    out << linesep << endl
        <<  "START TEST - " << name << endl
        << endl;
  }

  int end_test_suite(ostream &out, const string &name,
                     const int &correct, const int &total) {
    bool success = (correct == total && total > 0);
    int score = success ? 1 : 0;

    out << endl
        << "END TEST - " << name
        << ": " << (success ? "[SUCCESS]" : "[FAILURE]")
        // << " - " << "[score " << score
        << " (" << correct << " correct test cases out of " << total << ")" << endl
        << linesep << endl << endl;

    return score;
  }

  int compute_final_score(ostream &out,
                          const vector<pair<int,int>> scores,
                          const std::string test_names[]) {
    // compute the total score
    int score = 0;
    int total_points = 0;
    int total_score = 0;
    for (auto element : scores) {
      total_score += element.first * element.second;
      total_points += element.second;
    }

    score = total_score;

    out << linesep << endl << "Scores summary:" << endl << linesep << endl;
    int i = 0;
    for (auto element : scores) {
      i += 1;
      out << i << " " << test_names[i-1] << ": "
          << (element.first * element.second)
          << endl;
    }
    out << endl << "Total: " << total_score << endl << linesep << endl;

    return score;
  }

  int run_grading(std::ostream &out, const int test_case_number, int const total_test_cases,
                  std::string const test_names[],
                  int const points[],
                  int (*test_functions[]) (std::ostream &, const std::string)) {
    std::vector<pair<int,int>> scores;

    for (int i = 0; i < total_test_cases; ++i) {
      if (test_case_number <= 0 || (i+1) == test_case_number) {
        int test_result = (*test_functions[i])(out,
                                               test_names[i]);
        scores.push_back(pair<int,int>(test_result, points[i]));
      }
    }

    if (test_case_number <= 0) {
      compute_final_score(out, scores, test_names);
    }

    return 0;
  }


}



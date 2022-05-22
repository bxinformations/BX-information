#include <iostream>
#include "ComplexDouble.hpp"


ComplexDouble::ComplexDouble(double r, double i) {
  this->r=r; this->i=i;
}

double ComplexDouble::get_r() {
  return r;
}

double ComplexDouble::get_i() {
  return i;
}


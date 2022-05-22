#ifndef COMPLEX_DOUBLE_HPP
#define COMPLEX_DOUBLE_HPP


class ComplexDouble {
public:

  ComplexDouble(double r, double i);

  double get_r();
  double get_i();

private:
  double r; // the real part
  double i; // the imaginary part
};

#endif

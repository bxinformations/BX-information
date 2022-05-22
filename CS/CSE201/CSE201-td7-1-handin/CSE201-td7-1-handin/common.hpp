/* DO NOT MODIFY THIS FILE */

#ifndef COMMON_HPP
#define COMMON_HPP

#include <iostream>

class Coordinate {
public:
  Coordinate(double x_other, double y_other);
  Coordinate();
  Coordinate(const Coordinate &other);
  ~Coordinate();

  double get_x() const;
  double get_y() const;
  double get_distance(const Coordinate &other) const;

  void set_x(const double x);
  void set_y(const double y);

  Coordinate operator+(const Coordinate& other);
  Coordinate operator-();
  Coordinate operator-(const Coordinate& other);

  bool operator==(const Coordinate& other) const;
  bool operator!=(const Coordinate& other) const;
  bool operator>(const Coordinate& other) const;
  bool operator<(const Coordinate& other) const;

  friend std::ostream& operator<<(std::ostream& os, const Coordinate& c);

private:
  double x;
  double y;
};

class Projectile {
public:
  Projectile(Coordinate position_other, double magnitude, double angle);
  Projectile();
  virtual ~Projectile();

  Coordinate get_position() const;
  double get_velocity_x() const;
  double get_velocity_y() const ;
  void simulate_step(double simulation_interval);

  bool operator==(const Projectile& other);
  bool operator!=(const Projectile& other);

  friend std::ostream& operator<<(std::ostream& os, const Projectile& c);
private:
  Coordinate position;
  double velocity_x;
  double velocity_y;
  double init_magnitude;
  double init_angle;
};

class Target {
public:
  Target(Coordinate position_other, double radius);
  Target();

  Coordinate get_position() const;
  double get_radius() const;

  bool operator==(const Target& other);
  bool operator!=(const Target& other);

  virtual void simulate_step(double simulation_interval);

  friend std::ostream& operator<<(std::ostream& os, const Target& c);
private:
  Coordinate position;
  double radius;
};


#endif


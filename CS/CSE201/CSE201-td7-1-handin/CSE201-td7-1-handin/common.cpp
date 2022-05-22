/*
 * Contains common code --- do not modify this file
 */

#include "common.hpp"
#include <math.h>

std::ostream& operator<<(std::ostream& os, const Coordinate& c)
{
    os << "Coordinate(" << c.x << "," << c.y << ")";
    return os;
}

std::ostream& operator<<(std::ostream& os, const Target& c)
{
    os << "Target(Coordinate(" << c.get_position().get_x() << ","
       << c.get_position().get_y() << ")," << c.get_radius() << ")";
    return os;
}

std::ostream& operator<<(std::ostream& os, const Projectile& c)
{
    os << "Projectile(Coordinate(" << c.get_position().get_x() << ","
       << c.get_position().get_y() << ")," << c.init_magnitude << ","
       << c.init_angle << ")";
    return os;
}


Coordinate::Coordinate(double x_other, double y_other) : x{x_other}, y{y_other} {
}

Coordinate::Coordinate() : Coordinate(0,0) {
}

Coordinate::Coordinate(const Coordinate &other) {
  x = other.x;
  y = other.y;
}

Coordinate::~Coordinate() {}

double Coordinate::get_x() const {return x;}
double Coordinate::get_y() const {return y;}

void Coordinate::set_x(const double x) {this->x = x;}
void Coordinate::set_y(const double y) {this->y = y;}

Coordinate Coordinate::operator+(const Coordinate& other){
  return Coordinate(x + other.x, y + other.y);
}

Coordinate Coordinate::operator-() {
  return Coordinate(-x, -y);
}

Coordinate Coordinate::operator-(const Coordinate& other) {
  return Coordinate(x - other.x, y - other.y);
}

bool Coordinate::operator==(const Coordinate& other) const {
  return (x == other.x && y == other.y);
}

bool Coordinate::operator!=(const Coordinate& other) const {
  return ! ((*this) == other);
}

bool Coordinate::operator>(const Coordinate& other) const {
  return (x > other.x && y > other.y);
}

bool Coordinate::operator<(const Coordinate& other) const {
  return (x < other.x && y < other.y);
}

double Coordinate::get_distance(const Coordinate &other) const {
  return hypot(x - other.get_x(), y - other.get_y());
}


Projectile::Projectile(Coordinate position_other,
                       double magnitude,
                       double angle) {
  double PI = 3.14159265;

  position = position_other;
  velocity_x = magnitude * cos(angle * PI / 180);
  velocity_y = magnitude * sin(angle * PI / 180);
  init_magnitude = magnitude;
  init_angle = angle;
}

Projectile::Projectile() : Projectile(Coordinate(),1,45){

}

Projectile::~Projectile() {};

Coordinate Projectile::get_position() const {
  return position;
}

double Projectile::get_velocity_x() const {
  return velocity_x;
}

double Projectile::get_velocity_y() const {
  return velocity_y;
}

void Projectile::simulate_step(double time_interval) {
  const double g = 9.8;

  position = Coordinate(position.get_x() + velocity_x * time_interval,
                        position.get_y() + velocity_y * time_interval +
                        0.5 * (-g) * time_interval * time_interval);
  velocity_y = velocity_y - g * time_interval;
}

bool Projectile::operator==(const Projectile& other) {
  return (position.get_x() == other.position.get_x() &&
          position.get_y() == other.position.get_y() &&
          velocity_x == other.velocity_x &&
          velocity_y == other.velocity_y);
}

bool Projectile::operator!=(const Projectile& other) {
  return ! ((*this) == other);
}

Target::Target(Coordinate position_other, double radius) : position(position_other), radius(radius) {

}

Target::Target() : Target(Coordinate(0,0),1.0) {};

bool Target::operator==(const Target& other) {
  return (this->position == other.position &&
          this->radius == other.radius);
}

bool Target::operator!=(const Target& other) {
  return ! (*this == other);
}


Coordinate Target::get_position() const { return position; };
double Target::get_radius() const { return radius; };

void Target::simulate_step(double simulation_interval) {
  // do nothing...
  return;
}

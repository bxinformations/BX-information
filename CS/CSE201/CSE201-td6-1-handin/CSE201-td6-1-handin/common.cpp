/*
 * Contains common code --- do not modify this file
 */


#include "td6.hpp"
#include <math.h>

int count_coordinate_instances(Coordinate c1, Coordinate c2, double min_distance) {
 int num_instances = Coordinate::get_num_instances();
#if EXERCISE_4 == 1 || EXERCISE_5 == 1
 count_half_segments(c1,c2,min_distance);
#endif
 return Coordinate::get_num_instances() - num_instances;
}

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
  num_instances += 1;
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

bool Coordinate::operator==(const Coordinate& other) {
  return (x == other.x && y == other.y);
}

bool Coordinate::operator!=(const Coordinate& other) {
  return ! ((*this) == other);
}

bool Coordinate::operator>(const Coordinate& other) {
  return (x > other.x && y > other.y);
}

bool Coordinate::operator<(const Coordinate& other) {
  return (x < other.x && y < other.y);
}

int Coordinate::num_instances = 0;

int Coordinate::get_num_instances() { return num_instances; }

double Coordinate::get_distance(Coordinate other) {
  return hypot(x - other.get_x(), y - other.get_y());
}

Target::Target(Coordinate position_other, double radius) : position(position_other), radius(radius) {

}

Target::Target() : Target(Coordinate(0,0),1.0) {};

bool Target::operator==(const Target& other) {
  return (this->position == other.position &&
          this->radius == other.radius);
  // do not require to add status, but ok if the students do that
}

bool Target::operator!=(const Target& other) {
  return ! (*this == other);
}


Coordinate Target::get_position() const { return position; };
double Target::get_radius() const { return radius; };

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

double Projectile::get_velocity_x() {
  return velocity_x;
}

double Projectile::get_velocity_y() {
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



ProjectileListNode::ProjectileListNode(Projectile projectile) {
  this->next = NULL;
  this->prev = NULL;
  this->element = projectile;
}

Projectile ProjectileListNode::get_projectile() {
  return element;
}

void ProjectileListNode::set_next(ProjectileListNode* next) {
  this->next = next;
}

void ProjectileListNode::set_prev(ProjectileListNode* prev) {
  this->prev = prev;
}


ProjectileListNode* ProjectileListNode::get_next() {
  return next;
}

ProjectileListNode* ProjectileListNode::get_prev() {
  return prev;
}


ProjectileList::ProjectileList() {
  head = NULL;
  tail = NULL;
}

ProjectileList::~ProjectileList() {
  for (ProjectileListNode *current = head;
       current != NULL;) {
    ProjectileListNode* app;
    app = current;
    current = app->get_next();
    delete app;
  }
  head = NULL;
  tail = NULL;
}

bool ProjectileList::is_empty() {
  return head == NULL && tail == NULL;
}

void ProjectileList::append(Projectile projectile) {
  ProjectileListNode *new_element = new ProjectileListNode(projectile);
  new_element->set_next(NULL);
  new_element->set_prev(NULL);

  if (head == NULL && head == tail) {
    // empty list
    head = new_element;
    tail = new_element;
  } else {
    tail->set_next(new_element);
    new_element->set_prev(tail);
    tail = new_element;
  }
}

Projectile ProjectileList::remove_from_head() {
  Projectile p = head->get_projectile();

  if (head == tail) {
    delete head;
    head = NULL;
    tail = NULL;
  } else {
    ProjectileListNode* app;
    app = head;
    head = head->get_next();
    head->set_prev(NULL);
    delete app;
  }
  return p;
}

Projectile ProjectileList::remove_from_tail() {
  Projectile p = tail->get_projectile();

  if (head == tail) {
    delete tail;
    head = NULL;
    tail = NULL;
  } else {
    ProjectileListNode* app;
    app = tail;
    tail = tail->get_prev();
    tail->set_next(NULL);
    delete app;
  }
  return p;
}


// Target list

TargetListNode::TargetListNode(Target projectile) {
  this->next = NULL;
  this->prev = NULL;
  this->element = projectile;
}


Target TargetListNode::get_projectile() {
  return element;
}

void TargetListNode::set_next(TargetListNode* next) {
  this->next = next;
}

void TargetListNode::set_prev(TargetListNode* prev) {
  this->prev = prev;
}


TargetListNode* TargetListNode::get_next() {
  return next;
}

TargetListNode* TargetListNode::get_prev() {
  return prev;
}


TargetList::TargetList() {
  head = NULL;
  tail = NULL;
}

TargetList::~TargetList() {
  for (TargetListNode *current = head;
       current != NULL;) {
    TargetListNode* app;
    app = current;
    current = app->get_next();
    delete app;
  }
  head = NULL;
  tail = NULL;
}

bool TargetList::is_empty() {
  return head == NULL && tail == NULL;
}

void TargetList::append(Target projectile) {
  TargetListNode *new_element = new TargetListNode(projectile);
  new_element->set_next(NULL);
  new_element->set_prev(NULL);

  if (head == NULL && head == tail) {
    // empty list
    head = new_element;
    tail = new_element;
  } else {
    tail->set_next(new_element);
    new_element->set_prev(tail);
    tail = new_element;
  }
}

Target TargetList::remove_from_head() {
  Target p = head->get_projectile();

  if (head == tail) {
    delete head;
    head = NULL;
    tail = NULL;
  } else {
    TargetListNode* app;
    app = head;
    head = head->get_next();
    head->set_prev(NULL);
    delete app;
  }
  return p;
}

Target TargetList::remove_from_tail() {
  Target p = tail->get_projectile();

  if (head == tail) {
    delete tail;
    head = NULL;
    tail = NULL;
  } else {
    TargetListNode* app;
    app = tail;
    tail = tail->get_prev();
    tail->set_next(NULL);
    delete app;
  }
  return p;
}

#include <iostream>
#include <math.h>

#include "td5.hpp"

// -----------------------------------------------------------------------------
// Exercise 1
// -----------------------------------------------------------------------------

// Define the arithmetic operators

// -----------------------------------------------------------------------------
// Exercise 2
// -----------------------------------------------------------------------------

// Define the comparison operators

// -----------------------------------------------------------------------------
// Exercise 3
// -----------------------------------------------------------------------------

double Coordinate::mx = 0;
double Coordinate::my = 0;

void Coordinate::reset_max()
{
    mx = my = 0;
}

double Coordinate::get_x_max()
{
    return mx;
}

double Coordinate::get_y_max()
{
    return my;
}

#if EXERCISE_4 == 1
// -----------------------------------------------------------------------------
// Exercise 4
// -----------------------------------------------------------------------------

Coordinate DroppingProjectile::mx = Coordinate(0, 0);

DroppingProjectile::DroppingProjectile(Coordinate position_other,
                                       double magnitude,
                                       double angle): Projectile(position_other, magnitude, angle)
{
    mx = mx < position_other ? position_other : mx;
}

DroppingProjectile::DroppingProjectile() : Projectile()
{

}

DroppingProjectile::~DroppingProjectile()
{
    mx = Coordinate(0, 0);
}

void DroppingProjectile::simulate_step(double time_interval)
{
    if (get_velocity_y() <= 0)
    {
        velocity_x = 0;
    }
    Projectile::simulate_step(time_interval);
    mx = mx < get_position() ? get_position() : mx;
}

Coordinate DroppingProjectile::get_max_coordinate()
{
    return mx;
}

#endif

// -----------------------------------------------------------------------------
// Exercise 6
// -----------------------------------------------------------------------------

ListNode::ListNode(Projectile projectile)
{
    element = projectile;
    next = NULL;
}

Projectile ListNode::get_projectile()
{
    return element;
}

void ListNode::set_next(ListNode* next)
{
    this->next = next;
}

ListNode* ListNode::get_next()
{
    return next;
}


// -----------------------------------------------------------------------------
// Exercise 7
// -----------------------------------------------------------------------------

List::List()
{
    head = NULL;
    tail = NULL;
}

List::~List()
{
    delete head;
    delete tail;
}

bool List::is_empty()
{
    return head == NULL && tail == NULL;
}

void List::append(Projectile projectile)
{
    if (head == NULL)
    {
        head = new ListNode(projectile);
        tail = head;
    }
    else
    {
        tail->set_next(new ListNode(projectile));
        tail = tail->get_next();
    }
}

Projectile List::remove_from_top()
{
    Projectile p = head->get_projectile();
    if (head->get_next() == NULL)
    {
        head = NULL;
        tail = NULL;
    }
    else head = head->get_next();
    return p;
}


// -----------------------------------------------------------------------------
// Exercise 8
// -----------------------------------------------------------------------------

PtrListNode::PtrListNode(Projectile *projectile)
{
    element = projectile;
    next = NULL;
}

Projectile* PtrListNode::get_projectile()
{
    return element;
}

void PtrListNode::set_next(PtrListNode* next)
{
    this->next = next;
}


PtrListNode* PtrListNode::get_next()
{
    return next;
}

PtrList::PtrList()
{
    head = NULL;
    tail = NULL;
}

PtrList::~PtrList()
{
    delete head;
    delete tail;
}

bool PtrList::is_empty()
{
    return head == NULL && tail == NULL;
}

void PtrList::append(Projectile *projectile)
{
    if (head == NULL)
    {
        head = new PtrListNode(projectile);
        tail = head;
    }
    else
    {
        tail->set_next(new PtrListNode(projectile));
        tail = tail->get_next();
    }
}

Projectile* PtrList::remove_from_top()
{
    Projectile *p = head->get_projectile();
    if (head->get_next() == NULL)
    {
        head = NULL;
        tail = NULL;
    }
    else head = head->get_next();
    return p;
}


// -----------------------------------------------------------------------------
// DO NOT MODIFY THE FOLLOWING CODE
// -----------------------------------------------------------------------------

double Coordinate::get_distance(Coordinate other) {
  return hypot(x - other.get_x(), y - other.get_y());
}

Projectile::Projectile(Coordinate position_other,
                       double magnitude,
                       double angle) {
  double PI = 3.14159265;

  position = position_other;
  velocity_x = magnitude * cos(angle * PI / 180);
  velocity_y = magnitude * sin(angle * PI / 180);
}

Projectile::Projectile() : Projectile(Coordinate(),1,45){

}

Projectile::~Projectile() {};

Coordinate Projectile::get_position() {
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

void simulate_full_trajectory(std::ostream &out, double simulation_interval,
                              Projectile *projectile_ptr) {
  while (projectile_ptr->get_position().get_y() >= 0)
    projectile_ptr->simulate_step(simulation_interval);
}

#if EXERCISE_4 == 1
void simulate_full_trajectory(std::ostream &out, double simulation_interval,
                              DroppingProjectile &projectile) {
  while (projectile.get_position().get_y() >= 0)
    projectile.simulate_step(simulation_interval);
}

#endif

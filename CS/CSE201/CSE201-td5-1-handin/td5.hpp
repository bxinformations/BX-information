#ifndef TD5_HPP
#define TD5_HPP

#define EXERCISE_1 1
#define EXERCISE_2 1
#define EXERCISE_3 1
#define EXERCISE_4 1
#define EXERCISE_5 1
#define EXERCISE_6 1
#define EXERCISE_7 1
#define EXERCISE_8 1


#include <assert.h>

// -----------------------------------------------------------------------------
// Exercise 1, 2, and 3
// Modify the Coordinate class
// -----------------------------------------------------------------------------
class Coordinate
{
public:
    Coordinate() : x{0}, y{0} { }
    Coordinate(double x_other, double y_other) : x{x_other}, y{y_other}
    {
        mx = mx > x ? mx : x;
        my = my > y ? my : y;
    };

    double get_x() const {return x;}
    double get_y() const {return y;}
    double get_distance(Coordinate other);

    Coordinate operator + (const Coordinate &a)
    {
        return Coordinate(x + a.x, y + a.y);
    };

    Coordinate operator - (const Coordinate &a)
    {
        return Coordinate(x - a.x, y - a.y);
    };

    Coordinate operator -()
    {
        return Coordinate(-x, -y);
    };

    bool operator == (const Coordinate &a)
    {
        return (x == a.x) && (y == a.y);
    };

    bool operator != (const Coordinate &a)
    {
        return !((x == a.x) && (y == a.y));
    };

    bool operator > (const Coordinate &a)
    {
        return x > a.x && y > a.y;
    };

    bool operator < (const Coordinate &a)
    {
        return x < a.x && y < a.y;
    };

    static void reset_max();
    static double get_x_max();
    static double get_y_max();

private:
    double x;
    double y;

    static double mx;
    static double my;
};

#include <iostream>

// -----------------------------------------------------------------------------
// Exercise 4
// Modify the Projectile class and complete the implementation of the
// DroppingProjectile class
// -----------------------------------------------------------------------------

class Projectile
{
public:
    Projectile(Coordinate position_other, double magnitude, double angle);
    Projectile();
    virtual ~Projectile();

    Coordinate get_position();
    double get_velocity_x();
    double get_velocity_y();
    virtual void simulate_step(double simulation_interval);
    bool operator==(const Projectile& other);
    double velocity_x;

private:
    Coordinate position;
    double velocity_y;
};

void simulate_full_trajectory(std::ostream &out, double simulation_interval,
                              Projectile *projectile_ptr);

// -----------------------------------------------------------------------------
// Exercise 4
// Complete the implementation of the DroppingProjectile class
// -----------------------------------------------------------------------------
#if EXERCISE_4 == 1   // DO NOT CHANGE THIS

class DroppingProjectile: public Projectile
{
public:
    DroppingProjectile(Coordinate position_other,
                       double magnitude,
                       double angle);
    DroppingProjectile();
    virtual ~DroppingProjectile();
    virtual void simulate_step(double time_interval);
    Coordinate get_max_coordinate();
private:
    static Coordinate mx;
};

void simulate_full_trajectory(std::ostream &out, double simulation_interval,
                              DroppingProjectile &projectile);
#endif // DO NOT CHANGE THIS


// -----------------------------------------------------------------------------
// Exercise 6
// Implements the functions for the ListNode class in td5.cpp
// -----------------------------------------------------------------------------
class ListNode
{
public:
    ListNode(Projectile projectile);

    Projectile get_projectile();
    void set_next(ListNode* next);
    ListNode* get_next();

private:
    Projectile element;
    ListNode *next;
};

// -----------------------------------------------------------------------------
// Exercise 7
// Implements the functions for the List class in td5.cpp
// -----------------------------------------------------------------------------
class List
{
public:
    List();
    ~List();

    bool is_empty();
    void append(Projectile projectile);
    Projectile remove_from_top();

private:
    ListNode *head, *tail;
};


// -----------------------------------------------------------------------------
// Exercise 8
// Implements the functions for the PtrListNode and PtrList class in td5.cpp
// -----------------------------------------------------------------------------
class PtrListNode {
public:
  PtrListNode(Projectile *projectile);
  Projectile* get_projectile();
  void set_next(PtrListNode* next);
  PtrListNode* get_next();
private:
  Projectile *element;
  PtrListNode *next;
};


class PtrList {
public:
  PtrList();
  ~PtrList();
  bool is_empty();
  void append(Projectile *projectile);
  Projectile* remove_from_top();

private:
  PtrListNode *head, *tail;
};

#endif // TD5_HPP


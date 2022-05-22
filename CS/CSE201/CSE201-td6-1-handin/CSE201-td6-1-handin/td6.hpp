#ifndef TD6_HPP
#define TD6_HPP

// Enable the automatic grader for each exercises
#define EXERCISE_1 1
#define EXERCISE_2 1
#define EXERCISE_3 1
#define EXERCISE_4 1
#define EXERCISE_5 1
#define EXERCISE_6 1
#define EXERCISE_7 1
#define EXERCISE_8 1

#include <iostream>
#include "common.hpp"

// Exercise 1: declare the TargetStatus enum here

enum TargetStatus
{
    destroyable,
    unbreakable,
    hidden,
};

class Target {
public:
  Target(Coordinate position_other, double radius);
  Target();

  Coordinate get_position() const;
  double get_radius() const;

  bool operator==(const Target& other);
  bool operator!=(const Target& other);

  TargetStatus get_status()
  {
      return status;
  }

  void set_status(TargetStatus status)
  {
      this->status = status;
  }

  virtual void simulate_step(double simulation_interval);

  friend std::ostream& operator<<(std::ostream& os, const Target& c);
private:
  Coordinate position;
  double radius;

  TargetStatus status;
};


Coordinate halve_distance(const Coordinate &c1, const Coordinate &c2);

// Exercise 5: reduce the number of Coordinate instances
int count_half_segments(const Coordinate& start, const Coordinate& end, double min_distance);


template <typename T1, typename T2> double get_distance(T1 a, T2 b)
{
    Coordinate c1 = a.get_position();
    Coordinate c2 = b.get_position();
    return c1.get_distance(c2);
}


// Declaration of the target list (the projectile list is in common.hpp)
class TargetListNode {
public:
  TargetListNode(Target projectile);

  Target get_projectile();
  void set_next(TargetListNode* next);
  void set_prev(TargetListNode* prev);
  TargetListNode* get_next();
  TargetListNode* get_prev();

private:
  Target element;
  TargetListNode *next, *prev;
};

class TargetList {
public:
  TargetList();
  ~TargetList();

  bool is_empty();
  void append(Target projectile);
  Target remove_from_head();
  Target remove_from_tail();

private:
  TargetListNode *head, *tail;
};


// Function templates for list operations
template<typename ListType> void init_list(ListType *&list) {
  list = new ListType();
}

template<typename ListType, typename ElementType>
void append(ListType* list,
            ElementType element) {
  list->append(element);
}


template<typename l> bool is_list_empty(l list)
{
    return list->is_empty();
}

template<typename ListType, typename ElementType>
void remove_from_head(ListType* list,
            ElementType &element)
{
    element = list->remove_from_head();
}

template<typename ListType, typename ElementType>
void remove_from_tail(ListType* list,
            ElementType &element)
{
    element = list->remove_from_tail();
}

template<typename ListType, typename ElementType>
void copy(ListType* source, ListType *&destination, ElementType to_exclude)
{
    ElementType m;
    init_list(destination);
    while (!is_list_empty(source))
    {
        remove_from_head(source, m);
        if (m != to_exclude)
        {
            append(destination, m);
        }
    }
}

#endif // TD6_HPP



#ifndef COORDINATE_LIST_HPP
#define COORDINATE_LIST_HPP

#include "common.hpp"

class CoordinateListNode {
public:
  CoordinateListNode(Coordinate element);

  Coordinate get_element();
  void set_next(CoordinateListNode* next);
  void set_prev(CoordinateListNode* prev);
  CoordinateListNode* get_next();
  CoordinateListNode* get_prev();

private:
  Coordinate element;
  CoordinateListNode *next, *prev;
};

class CoordinateList {
public:
  CoordinateList();
  ~CoordinateList();

  bool is_empty();
  void append(Coordinate element);
  Coordinate remove_from_head();
  Coordinate remove_from_tail();

private:
  CoordinateListNode *head, *tail;
};



#endif // TD7_HPP

#include "coordinatelist.hpp"

CoordinateListNode::CoordinateListNode(Coordinate element) {
  this->next = NULL;
  this->prev = NULL;
  this->element = element;
}

Coordinate CoordinateListNode::get_element() {
  return element;
}

void CoordinateListNode::set_next(CoordinateListNode* next) {
  this->next = next;
}

void CoordinateListNode::set_prev(CoordinateListNode* prev) {
  this->prev = prev;
}

CoordinateListNode* CoordinateListNode::get_next() {
  return next;
}

CoordinateListNode* CoordinateListNode::get_prev() {
  return prev;
}



CoordinateList::CoordinateList() {
  head = NULL;
  tail = NULL;
}

CoordinateList::~CoordinateList() {
  for (CoordinateListNode *current = head;
       current != NULL;) {
    CoordinateListNode* app;
    app = current;
    current = app->get_next();
    delete app;
  }
  head = NULL;
  tail = NULL;
}

bool CoordinateList::is_empty() {
  return head == NULL && tail == NULL;
}

void CoordinateList::append(Coordinate element) {
  CoordinateListNode *new_element = new CoordinateListNode(element);
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

Coordinate CoordinateList::remove_from_head() {
  Coordinate p = head->get_element();

  if (head == tail) {
    delete head;
    head = NULL;
    tail = NULL;
  } else {
    CoordinateListNode* app;
    app = head;
    head = head->get_next();
    head->set_prev(NULL);
    delete app;
  }
  return p;
}

Coordinate CoordinateList::remove_from_tail() {
  Coordinate p = tail->get_element();

  if (head == tail) {
    delete tail;
    head = NULL;
    tail = NULL;
  } else {
    CoordinateListNode* app;
    app = tail;
    tail = tail->get_prev();
    tail->set_next(NULL);
    delete app;
  }
  return p;
}

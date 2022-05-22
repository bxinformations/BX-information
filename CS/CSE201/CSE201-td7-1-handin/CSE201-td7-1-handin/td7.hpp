#ifndef TD7_HPP
#define TD7_HPP

#include "common.hpp"
#include <vector>

#define EXERCISE_1 1
#define EXERCISE_2 1
#define EXERCISE_3 1
#define EXERCISE_4 1
#define EXERCISE_5 1
#define EXERCISE_6 1

template <class T>
class Complex
{
public:
    Complex(T r, T i)
    {
        this->r = r;
        this->i = i;
    }
    T get_r()
    {
        return r;
    }
    T get_i()
    {
        return i;
    }
private:
    T r,i;
};

template <class T>
class ListNode
{
public:
    ListNode(T element)
    {
        this->element = element;
        this->next = NULL;
        this->prev = NULL;
    }

    T get_element()
    {
        return element;
    }
    void set_next(ListNode* next)
    {
        this->next = next;
    }
    void set_prev(ListNode* prev)
    {
        this->prev = prev;
    }
    ListNode* get_next()
    {
        return next;
    }
    ListNode* get_prev()
    {
        return prev;
    }

private:
    T element;
    ListNode *next, *prev;
};

template <class T>
class List
{
public:
    List()
    {
        head = NULL;
        tail = NULL;
    }
    ~List()
    {
        for (ListNode<T> *current = head;
            current != NULL;)
        {
            ListNode<T>* app;
            app = current;
            current = app->get_next();
            delete app;
        }
        head = NULL;
        tail = NULL;
    }

    bool is_empty()
    {
        return head == NULL && tail == NULL;
    }
    void append(T element)
    {
        ListNode<T> *new_element = new ListNode<T>(element);
        new_element->set_next(NULL);
        new_element->set_prev(NULL);

        if (head == NULL && head == tail)
        {
            // empty list
            head = new_element;
            tail = new_element;
        }
        else
        {
            tail->set_next(new_element);
            new_element->set_prev(tail);
            tail = new_element;
        }
    }
    T remove_from_head()
    {
        T p = head->get_element();

        if (head == tail)
        {
            delete head;
            head = NULL;
            tail = NULL;
        }
        else
        {
            ListNode<T>* app;
            app = head;
            head = head->get_next();
            head->set_prev(NULL);
            delete app;
        }
        return p;
    }
    T remove_from_tail()
    {
        T p = tail->get_element();

        if (head == tail)
        {
            delete tail;
            head = NULL;
            tail = NULL;
        }
        else
        {
            ListNode<T>* app;
            app = tail;
            tail = tail->get_prev();
            tail->set_next(NULL);
            delete app;
        }
        return p;
    }

private:
    ListNode<T> *head, *tail;
};

#if EXERCISE_4 == 1
std::vector<int> reverse_even(std::vector<int> input_vector);
#endif

#if EXERCISE_5 == 1
std::vector<Coordinate> same_coordinates(std::vector<double> list_of_x,
                                         std::vector<double> list_of_y);
#endif

#if EXERCISE_6 == 1
std::vector<int> filter_max(std::vector<int> max_vector,
                            std::vector<int> other_vector);
#endif



#endif // TD7_HPP

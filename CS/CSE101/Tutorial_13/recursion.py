#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan  9 10:10:50 2020

@author: peixin.you
"""

def gcd(a, b):
    if (b == 0):
        return a
    return gcd(b, a % b)

def is_palindrome(word):
    """Check if input is a palindrome."""
    if (word == '' or len(word) == 1):
        return True
    if (word[0] != word[-1]):
        return False
    return is_palindrome(word[1:-1])

def binary_search(sorted_list, lower, upper, element):
    """Return the position of the element in the sublist of sorted_list 
    starting at position lower up to (but excluding) position upper if it 
    appears there. Otherwise return -1.
    """
    upper -= 1
    if (lower >= upper):
        if (sorted_list[upper] == element):
            return lower
        return -1
    mid = (lower + upper) >> 1
    if (sorted_list[mid] < element):
        return binary_search(sorted_list, mid + 1, upper + 1, element)
    else:
        return binary_search(sorted_list, lower, mid + 1, element)

pos = -1

numbers = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

def read_positive_integer(text, position):
    """Read a number starting from the given position, return it and the first
    position after it in a tuple. If there is no number at the given position
    then return None.
    """
    
    global pos
    if (pos == -1):
        pos = position
    if (position == len(text)):
        if pos >= len(text):
            return None
        return (int(text[pos:]), len(text[pos:]) + pos)
    if (text[position] not in numbers):
        if (pos == position):
            return None
        return (int(text[pos:position]), len(text[pos:position]) + pos)
    else:
        return read_positive_integer(text, position + 1)
    
def cal(a, b, oper):
    if (oper == '+'):
        return a + b
    if (oper == '-'):
        return a - b
    if (oper == '*'):
        return a * b
    if (oper == '/'):
        return a / b
    
def deci(top_op, now_op):
    rate1 = ['+', '-']
    rate2 = ['*', '/']
    rate3 = ['(']
    rate4 = [')']
    if (top_op in rate1):
        if (now_op in rate3 or now_op in rate2):
            return -1
        else:
            return 1
    elif top_op in rate2:
        if (now_op in rate3):
            return -1
        else:
            return 1
    elif top_op in rate3:
        if now_op in rate4:
            return 0
        else:
            return -1
    return -1
    
operator = ['+', '-', '*', '/', '(', ')']

def evaluate(expression, position):
    """Evaluate the expression starting from the given position. Return
    the value and the first position after the read sub-expression. If the 
    string starting at the given expression is not an arithmetic expression, 
    return None.
    """
    formula = []
    while (position < len(expression)):
        if (expression[position] in numbers):
            global pos
            pos = -1
            now, position = read_positive_integer(expression, position)
            formula.append(now)
        else:
            formula.append(expression[position])
            position += 1
    num_stack = []
    op_stack = []
    for e in formula:
        if e not in operator:
            num_stack.append(e)
        else:
            while True:
                if (len(op_stack) == 0):
                    op_stack.append(e)
                    break
                f = deci(op_stack[-1], e)
                if f == -1:
                    op_stack.append(e)
                    break
                if f == 0:
                    op_stack.pop()
                    break
                if f == 1:
                    op = op_stack.pop()
                    a = num_stack.pop()
                    b = num_stack.pop()
                    num_stack.append(cal(b, a, op))
    while len(op_stack) != 0:
        op = op_stack.pop()
        b = num_stack.pop()
        a = num_stack.pop()
        num_stack.append(cal(a, b, op))
    return (num_stack[0], position)
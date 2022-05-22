#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 10:17:14 2020

@author: peixin.you
"""

class Node:
    def __init__(self, key, value, left, right):
        self.key = key
        self.value = value
        self.left = left
        self.right = right

    def __repr__(self):
        return 'Node({}, {}, {}, {})'.format(
            repr(self.key),
            repr(self.value),
            repr(self.left),
            repr(self.right))

    def __str__(self):
        return 'Tree node; key: {}, value: {}'.format(
            repr(self.key),
            repr(self.value))
        
    def search(self, key):
        if (key == self.key):
            return self.value
        if (self.key > key):
            if (self.left == None):
                return None
            return self.left.search(key)
        else:
            if (self.right == None):
                return None
            return self.right.search(key)
    
    def print_in_order(self):
        if self.left != None:
            self.left.print_in_order()
        print('{}: {}'.format(self.key, self.value))
        if (self.right != None):
            self.right.print_in_order()
    
    def add(self, key, value):
        if (self.key == key):
            if (value not in self.value):
                self.value.append(value)
        elif (key < self.key):
            if (self.left == None):
                self.left = Node(key, [value], None, None)
            else:
                self.left.add(key, value)
        else:
            if (self.right == None):
                self.right = Node(key, [value], None, None)
            else:
                self.right.add(key, value)

    def write_in_order_rec(self, file):
        if self.left != None:
            self.left.write_in_order_rec(file)
        print('{}: {}'.format(self.key, self.value), file = file)
        if (self.right != None):
            self.right.write_in_order_rec(file)

    def write_in_order(self, filename):
        """Write all key:value pairs in the index tree
        to the named file, one entry per line.
        """
        with open(filename, 'w') as file:
            self.write_in_order_rec(file)    
            
    def height(self):
        if (self.left == None and self.right == None):
            return 0
        if (self.left == None):
            return self.right.height() + 1
        if (self.right == None):
            return self.left.height() + 1
        return max(self.left.height(), self.right.height()) + 1
    
def example_bst():
    return Node(8, 'Eight', Node(4, 'Four', Node(3, 'Three', None, None), \
                                 Node(6, 'Six', None, Node(7, 'Seven', None, None))), \
                            Node(10, 'Ten', None, Node(14, 'Fourteen', Node(13, 'Thirteen', None, None), None)))

def split_in_words_and_lowercase(line):
    """Given a line of text, return a list of non-empty lower-cased words in that line
    after all "'" and '-' characters have been converted to spaces, and all
    all punctuation on word boundaries has been removed.
    """
    parts = line.strip().replace('-', ' ').replace("'", ' ').split()
    parts = [p.strip('",._;?!:()[]').lower() for p in parts]
    return [p for p in parts if p != '']

def construct_bst_for_indexing(filename):
    i = 1
    result = None
    with open(filename, 'r') as f:
        for line in f:
            l = split_in_words_and_lowercase(line)
            for w in l:
                if result == None:
                    result = Node(w, [i], None, None)
                result.add(w, i)
            i += 1
    return result

def generate_index(filename):
    root = construct_bst_for_indexing(filename)
    root.write_in_order(filename + '.index')
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 13 09:46:54 2020

@author: peixin.you
"""

class A:
    def __init__(self, name, fwd):
        self.name = name
        self.fwd = fwd

    def __eq__(self, other):
        return self.name == other.name

class B:
    def __init__(self, name, bwd):
        self.name = name
        self.bwd = bwd
    def __eq__(self, other):
        return self.name == other.name

class Rdict:
    def __init__(self):
        self.__a = []
        self.__b = []
        self.len = 0
        
    def associate(self, a, b):
        if (A(a, b) in self.__a) or (B(b, a) in self.__b):
            return False
        self.__a.append(A(a, b))
        self.__b.append(B(b, a))
        self.len += 1
        
    def __len__(self):
        return self.len
    
    def __getitem__(self, key):
        if key[0] < 0:
            for i in self.__b:
                if key[1] == i.name:
                    return i.bwd
        else:
            for i in self.__a:
                if key[1] == i.name:
                    return i.fwd

    def __setitem__(self, key, value):
        if key[0] < 0:
            self.associate(value, key[1])
        else:
            self.associate(key[1], value)
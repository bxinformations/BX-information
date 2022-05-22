#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 13 08:03:01 2020

@author: peixin.you
"""

from math import gcd

class Fraction:
    def __init__(self, numerator = 1, denominator = 1):
        self.__numerator = numerator
        self.__denominator = denominator
    
    def reduce(self):
        d = gcd(self.__numerator, self.__denominator)
        if self.__denominator < 0:
            d = -d
        self.__numerator //= d
        self.__denominator //= d
        
    @property
    def numerator(self):
        d = gcd(self.__numerator, self.__denominator)
        if self.__denominator < 0:
            d = -d
        return self.__numerator // d
        
    @property
    def denominator(self):
        d = gcd(self.__numerator, self.__denominator)
        if self.__denominator < 0:
            d = -d
        return self.__denominator // d
    
    def __repr__(self):
        return 'Fraction(%d, %d)' % (self.__numerator, self.__denominator)
    
    def __str__(self):
        return '%d/%d' % (self.numerator, self.denominator)
    
    def __eq__(self, other):
        return (self.denominator == other.denominator) and (self.numerator == other.numerator)
    
    def __neg__(self):
        return Fraction(-self.numerator, self.denominator)
        
    def __add__(self, other):
        ans = Fraction(self.__numerator * other.__denominator + 
                       other.__numerator * self.__denominator, 
                       other.__denominator * self.__denominator)
        ans.reduce()
        return ans
    
    def __sub__(self, other):
        return self + (-other)
    
    def __mul__(self, other):
        ans = Fraction(self.__numerator * other.__numerator, 
                       self.__denominator * other.__denominator)
        ans.reduce()
        return ans
    
    def __truediv__(self, other):
        return self * Fraction(other.__denominator, other.__numerator)
    
    
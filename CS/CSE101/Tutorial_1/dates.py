#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 26 10:25:32 2019

@author: peixin.you
"""

def name_of_month(m):
    """Given an integer m between 1 and 12 inclusive,
    indicating a month of the year, returns the name of that month.
    For example: name_of_month(1) == 'January' and name_of_month(12) == 'December'.
    If the month does not exist (that is, if m is outside the legal range),
    then this function returns None.
    """
    if m < 1 or m > 12:  # Non-existent month
        return None
    elif m == 1:
        return 'January'
    elif m == 2:
        return 'February'
    elif m == 3:
        return 'March'
    elif m == 4:
        return 'April'
    elif m == 5:
        return 'May'
    elif m == 6:
        return 'June'
    elif m == 7:
        return 'July'
    elif m == 8:
        return 'August'
    elif m == 9:
        return 'September'
    elif m == 10:
        return 'October'
    elif m == 11:
        return 'November'
    else:
        return 'December'

def str_with_suffix(n):
    """Convert the integer n to a string expressing the corresponding 
    position in an ordered sequence.
    Eg. 1 becomes '1st', 2 becomes '2nd', etc.
    """
    s = str(n)
    a = n % 10
    n = n // 10
    b = n % 10
    if b == 1:
        return s + 'th'
    if a == 1:
        return s + 'st'
    if a == 2:
        return s + 'nd'
    if a == 3:
        return s + 'rd'
    return s + 'th'

def is_leap_year(y):
    """ Return True if y is a leap year, False otherwise. """
    n = y
    a = n % 10
    n = n // 10
    b = n % 10
    if a == 0 and b == 0:
        return (y % 400) == 0
    return (y % 4) == 0

def number_of_days(m, y):
    """Returns the number of days in month m of year y."""
    f = is_leap_year(y)
    big = [1, 3, 5, 7, 8, 10, 12]
    if m == 2 and f:
        return 29
    if m == 2 and not f:
        return 28
    if m in big:
        return 31
    return 30

def date_string(d, m, y):
    if name_of_month(m) == None:
        return 'Nonexistent date'
    day = number_of_days(m, y)
    if d < 1 or d > day:
        return 'Nonexistent date'
    return 'The ' + str_with_suffix(d) + ' of ' + name_of_month(m) + ', ' + str(y)

def time_string(t):
    
    if t < 60:
        if t == 1:
            return str(t) + ' second'
        else:
            return str(t) + ' seconds'
    m = t // 60
    t = t % 60
    if m < 60:
        if t == 1:
            s = str(t) + ' second'
        else:
             s = str(t) + ' seconds'
             if s == 0:
                 s = ''
        if m == 1:
            return str(m) + ' minute, ' + s
        else:
            return str(m) + ' minutes, ' + s
    h = m // 60
    m = m % 60
    if h < 24:
        if t == 1:
            s = str(t) + ' second'
        else:
            s = str(t) + ' seconds'
            if s == 0:
                 s = ''
        if m == 1:
            M = str(m) + ' minute, ' + s
        else:
            M = str(m) + ' minutes, ' + s
            if m == 0:
                M = s
        if h == 1:
            return str(h) + ' hour, ' + M
        else:
            return str(h) + ' hours, ' + M
    d = h // 24
    h = h % 24
    if t == 1:
        s = str(t) + ' second'
    else:
        s = str(t) + ' seconds'
        if s == 0:
            s = ''
    if m == 1:
        M = str(m) + ' minute, ' + s
    else:
        M = str(m) + ' minutes, ' + s
        if m == 0:
            M = s
    if h == 1:
        H = str(h) + ' hour, ' + M
    else:
        H = str(h) + ' hours, ' + M
        if h == 0:
            H = M
    if d == 1:
        return str(d) + ' day, ' + H
    return str(d) + ' days, ' + H

def play_hilo(x,n):
    """A simple guessing game.
    The player has to guess the target value x in at most n attempts.
    """
    print('Guess what number I am thinking of?')
    print('You have ' + str(n) + ' turns left')
    while n:
        print('')
        G = input('Guess:')
        if (G > x):
            print('Higher')
        elif (G < x):
            print('Lower')
        if (G == x):
            print('Congratulations!')
            break
        n = n - 1
        print('You have ' + str(n) + ' turns left')
    print('You lose!')
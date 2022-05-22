#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct  3 10:18:25 2019

@author: peixin.you
"""

import random


COLORS = ['1', '2', '3', '4', '5', '6','7']

lengthColors = 6

lengthGuess = 4
lengthCode = 4

def create_code():
    """Return 4-element list of strings randomly chosen from
    COLORS with repetition.
    """
    s=[]
    for i in range(lengthCode):
        s.append(random.choice(COLORS))    
    return s

def black_pins(guess, code):
    """guess, code: 4-element lists of strings from COLORS
    Returns the number of black pins, determined by the standard
    Mastermind rules, plus guess and code lists with matching
    pins removed
    """
    tot = 0
    for i in range(lengthCode):
        if guess[i] == code[i]:
            tot += 1
    s1 = []
    s2 = []
    for i in range(lengthCode):
        if (guess[i] != code[i]):
            s1.append(guess[i])
            s2.append(code[i])
    return tot , s1 , s2

def score_guess(guess, code):
    """guess, code: 4-element lists of strings
    Return (b, w) where
    b is the number of black pins (exact matches), and
    w is the number of white pins (correct colors in wrong places)
    """
    blacks, nguess, ncode = black_pins(guess, code)
    w = 0
    for i in range(lengthCode - blacks):
        if ncode[i] in nguess:
            w += 1
    return blacks, w

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

def input_guess():
    """Input four colors from COLORS and return as list.
    """
    print('Enter your guess:')
    n = 1
    guess = []
    while 1:
        pre = str_with_suffix(n)
        g = input(pre + 'pin: ')
        p = -1
        for i in range (lengthColors):
            if COLORS[i].startswith(g):
                if p == -1:
                    p = i
                else:
                    p = -1
                    break
        if p == -1:
            print('Please input an unambiguous prefix of a color from the list\n\
[\'RED\', \'GREEN\', \'BLUE\', \'PURPLE\',\'BROWN\', \'YELLOW\']')
            continue
        n += 1
        guess.append(g)
        if n == 5:
            return guess
    
def one_round(code):
    """Input guess, score guess, print result, and return True iff
    user has won.
    """
    guess = []
    guess = input_guess()
    b, w = score_guess(guess, code)
    print('Score: ' + str(b) + ' black, ' + str(w) + ' white')
    if b == lengthCode:
        return True
    else:
        return False
    
def play_mastermind():
    """Generate random Mastermind code and let user guess it in rounds
    """
    code = create_code()
    r = 1
    while 1:
        print('Round ' + str(r))
        r += 1
        if one_round(code):
            print('You win!')
            break

S = [[] for i in range(4096)]
tmp = 0

def create_S(dep):
    if dep == 5:
        tmp += 1
        return None
    for i in range(6):
        S[tmp].append(COLORS[i])
        create_S(dep + 1)

def autoplay_mastermind():
    now = ['1','1','2','2']
    code = create_code()
    create_S(0)
    while 1:
        B, W = score_guess(now,code)
        for i in range(tmp):
            if S == '':
                continue
            b, w = score_guess(S[i], code)
            if B != b or W != w:
                S[i] = ''
        
        for i in range(tmp):
            
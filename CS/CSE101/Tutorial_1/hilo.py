#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 26 11:32:27 2019

@author: peixin.you
"""

import random

def play_hilo(x, n):
    """A simple guessing game.
    The player has to guess the target value x in at most n attempts.
    """
    print('Guess what number I am thinking of?')
    print('You have ' + str(n) + ' turns left')
    G = input('Guess:')
    if (int(G) == x):
        print('Congratulations!')
        return
    print('Wrong')
    lastDis = abs(int(G) -x)
    n = n - 1
    while n:
        print('')
        G = input('Guess:')
        if (int(G) - x > lastDis):
            print('Getting colder')
        elif (int(G) - x < lastDis):
            print('Getting warmer')
        elif (int(G) - x == lastDis):
            print('Getting nowhere')
        if (int(G) == x):
            print('Congratulations!')
            return
        n = n - 1
        if n != 0:
            print('You have ' + str(n) + ' turns left')
    print('You lose!')
    
def play_random_hilo(lower, upper, n):
    """Play the hilo game with the target value x chosen 
    randomly from the interval [lower,upper].
    """
    x = random.randint(lower, upper)
    play_hilo(x, n);
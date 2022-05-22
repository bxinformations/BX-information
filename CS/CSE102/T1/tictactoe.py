#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 13 08:43:26 2020

@author: peixin.you
"""

class Board:
    def __init__(self, n):
        """Initialize a new empty `n`x`n` board."""

        # remember the size of the board as the .n attribute
        # note: the number of cells is n * n.
        self.__n = n

        # whose turn is it to play.
        # use: player == 0, opponent == 1
        self.__turn = 0

        # the depth of the board, i.e., how many turns it has taken
        # to get here from an initial empty board.
        self.__depth = 0
        
        self.__board = [[None for col in range(n)] for row in range(n)]

    @property
    def n(self):
        return self.__n
        
    @property
    def turn(self):
        return self.__turn

    @property
    def depth(self):
        return self.__depth
    
    @property
    def board(self):
        return self.__board
    
    def __getitem__(self, key):
        return self.__board[key[0]][key[1]]
    
    def play(self, row, col):
        if self.board[row][col] != None:
            return None
        self.__board[row][col] = self.turn
        self.__turn ^= 1
        self.__depth += 1
        return self
    
    def __str__(self):
        return 'Round: {} Plyer: {} n: {}\n{}'.format(self.depth, self.turn, self.n, self.__board)
    
    def transfer(self, b):
        self.__n = b.n
        self.__turn = b.turn
        self.__board = [row[:] for row in b.board]
        return self
        
    def copy(self):
        result = Board(0).transfer(self)
        return result
    
    def plays(self):
        result = []
        n = self.n
        for i in range(n):
            for j in range(n):
                now = self.copy().play(i, j)
                if now != None:
                    result.append(((i, j), now))
        return result
    
    def player(self, x):
        if x == 0:
            return 'X'
        else:
            return 'O'
    
    def report(self):
        print('%dx%d game, %c to play, %d move(s) deep' % (self.n, self.n, self.player(self.turn), self.depth))
        for i in range(self.n):
            result = ''
            for j in range(self.n):
                if self[i, j] == None:
                    result += ' '
                else:
                    result += self.player(self[i, j])
            print(result)
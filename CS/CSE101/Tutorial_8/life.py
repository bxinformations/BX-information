#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 21 10:19:07 2019

@author: peixin.you
"""

class Point:
    """Encodes a live point in the Game of Life"""
    def __init__(self, x, y):
        """Initialize x- and y-coordinate of the point."""
        self.x = x
        self.y = y
        
    def __repr__(self):
        """Give a string representation of the Point for debugging."""
        (x, y) = (self.x, self.y)
        return "Point({}, {})".format(x,y)

    def __eq__(self, other): 
        """Compare two Points' x- and y-coordinates."""
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        """Compute a hash value for Points."""
        (x, y) = (self.x, self.y)
        return hash((x, y))
    
    def get_neighbors(self):
        """Return the neighbors of the Point as a set."""
        result = {Point(self.x - 1 + x, self.y - 1 + y) for x in range (3) for y in range (3)}
        result.remove(Point(self.x, self.y))
        return result
    
class Board:
    """Store the current board and manipulate it.
    """
    def __init__(self, sizex, sizey, points):
        """Initialize size and initial points."""
        self.points = points
        self.sizex = sizex
        self.sizey = sizey

    def is_legal(self, point):
        """Check if a given Point is on the board."""
        return 0 <= point.x and point.x < self.sizex and 0 <= point.y  and point.y < self.sizey

    def number_live_neighbors(self, p):
        """Compute the number of neighbors of p on the Board that are alive."""
        neighbor = p.get_neighbors()
        return len(neighbor.intersection(self.points))
    
    def next_step(self):
        """Compute the points alive in the next round and update the 
        points of the Board.
        """
        result = set()
        for i in range(self.sizex):
            for j in range (self.sizey):
                now_point = Point(i, j)
                if self.number_live_neighbors(now_point)  == 3:
                    result.add(now_point)
                if self.number_live_neighbors(now_point)  == 2 and now_point in self.points:
                    result.add(now_point)
        self.points = result
        
    def load_from_file(self, filename):
        """Load a board configuration from file. The file format is as follows:
        - The first two lines contain a number representing the size in x- and 
            y-coordinates, respectively.
        - Each of the following lines gives the coordinates of a single point,
            with the two coordinate values separated by a comma.
            Those are the points that are alive in the board to be loaded.
        """
        tot = 1
        points = set()
        x = self.sizex
        y = self.sizey
        with open(filename) as input_file:
            for line in input_file:
                date = line
                if tot == 1:
                    x = int(date)
                elif tot == 2:
                    y = int(date)
                else:
                    date = date.split(',')
                    point = Point(int(date[0]), int(date[1]))
                    points.add(point)
                tot += 1
        self.sizex, self.sizey, self.points = (x, y, points)
        
    def toggle_point(self, x, y):
        """Add Point(x,y) if it is not in points, otherwise delete it from 
        points.
        """
        if Point(x, y) in self.points:
            self.points.remove(Point(x, y))
        else:
            self.points.add(Point(x, y))

    def save_to_file(self, filename):
        """Save a board to a file. The format is that described for 
        load_from_file().
        """
        output_file = open(filename,"w")
        output_file.writelines("{}\n".format(self.sizex))
        output_file.writelines("{}\n".format(self.sizey))
        for i in self.points:
            output_file.writelines("{},{}\n".format(i.x, i.y))
            
class TextView:
    """A text visualization of Board instances.
    """
    def __init__(self, board):
        # Initialize the board...
        self.board = board
    
    def show(self):
        """Print out a textual representation of the board.
        """
        result = ''
        for i in range(2 + self.board.sizex):
            for j in range(2 + self.board.sizey):
                if j == 0:
                    result += 'o'
                    continue
                if j == 1 + self.board.sizey:
                    result += 'o\n'
                    continue
                if i == 0 or i == 1 + self.board.sizex:
                    result += 'o'
                    continue
                if (Point(i - 1, j - 1) in self.board.points):
                    result += 'X'
                else:
                    result += ' '
        print(result)
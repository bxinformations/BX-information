#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 28 10:09:58 2019

@author: peixin.you
"""

class Ship:
    """A ship that can be placed on the grid"""

    def __repr__(self):
        """Give a representation for debugging."""
        return "Ship('{}', {})".format(self.name, self.positions)

    def __init__(self, name, positions):
        """Initialize name and position. Set hits to empty set."""
        self.name = name
        self.positions = positions
        self.hits = set()
        
    def __eq__(self, other):
        (n, p, h) = other.name, other.positions, other.hits
        return (self.hits == h) and (self.positions == p) and (self.name == n)
    
    def is_afloat(self):
        if self.hits == self.positions:
            return True
        else:
            return False
    
    def shoot_at_ship(self, shot):
        """Check if the shot hits the ship. If so, remember the hit.
        Returns one of 'MISS', 'HIT', or 'DESTROYED'.
        """
        if shot in self.positions - self.hits:
            self.hits.add(shot)
            if self.hits == self.positions:
                return 'DESTROYED'
            return 'HIT'
        else:
            return 'MISS'
    
class Grid:
    """Encodes the grid on which the Ships are placed. Also remembers the 
    shots that have been fired so far and if they were hits.
    """
    def __init__(self, sizex, sizey):
        """Initializes a board of the given size."""
        self.sizex = sizex
        self.sizey = sizey
        self.ships = []
        self.misses = set()
        self.hits = set()

    def add_ship(self, ship):
        """Add a Ship to the grid."""
        self.ships.append(ship)
        
    def shoot(self, pos):
        f = 0
        for i in self.ships:
            ans = i.shoot_at_ship(pos)
            if ans == 'HIT':
                f = 1
                self.hits.add(pos)
                return ('HIT', None)
            elif ans == 'MISS':
                pass
            else:
                f = 1
                self.hits.add(pos)
                return (ans, i)
        if f == 0:
            self.misses.add(pos)
            return ('MISS', None)

def create_ship_from_line(line):
    line = line.split(' ')
    p = set()
    name = line[0]
    for i in range(len(line) - 1):
        pos = line[i + 1]
        pos = pos.split(':')
        p.add((int(pos[0]), int(pos[1])))
    return Ship(name, p)

def load_grid_from_file(file_name):
    with open(file_name, "r") as open_file:
        i = 0
        sizex = 0
        sizey = 0
        g = Grid(sizex, sizey)
        for line in open_file:
            now = str(line[:-1])
            if i == 0:
                now = now.split(':')
                g.sizex = int(now[0])
                g.sizey = int(now[1])
            else:
                g.add_ship(create_ship_from_line(line))
            i += 1
        return g
    
class BlindGrid:
    """Encodes the opponent's view of the grid."""

    def __init__(self, grid):
        """Given a grid, initializes hits, misses and sunken ships."""
        self.sizex = grid.sizex
        self.sizey = grid.sizey
        self.misses = grid.misses
        self.hits = grid.hits
        self.sunken_ships = []
        for i in grid.ships:
            if i.is_afloat():
                self.sunken_ships.append(i)

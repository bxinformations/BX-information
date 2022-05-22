# -*- coding: utf-8 -*-
from Sat import *

class LatinSquarePuzzle:
    def __init__(self,k,initial):
      self.k=k  
      self.initial=initial
      self.sat=Sat(self.k**3,[])   
      self.final=[]
      self.sudoku_mode=False
        
    def triple_to_int(self,v,i,j):
        return 1+i*self.k**2+j*self.k+v
        
    def int_to_triple(self,r):    
        v=(r-1)%self.k
        j=((r-1)//self.k)%self.k
        i=((r-1)//self.k**2)%self.k
        return [v,i,j]
        
    def build_generic_clauses(self):
        for i in range(self.k):
            for j in range(self.k):
                l = []
                for v in range(self.k):
                    l.append(self.triple_to_int(v, i, j))
                self.sat.clauses.append(l)

        for v1 in range(self.k - 1):
            for v2 in range(v1 + 1, self.k):
                for i in range(self.k):
                    for j in range(self.k):
                        l = []
                        l.append(-self.triple_to_int(v1, i, j))
                        l.append(-self.triple_to_int(v2, i, j))
                        self.sat.clauses.append(l)

        for v in range(self.k):
            for i in range(self.k):
                l = []
                for j in range(self.k):
                    l.append(self.triple_to_int(v, i, j))
                self.sat.clauses.append(l)

        for v in range(self.k):
            for i in range(self.k):
                for j1 in range(0, self.k - 1):
                    for j2 in range(j1 + 1, self.k):
                        l = []
                        l.append(-self.triple_to_int(v, i, j1))
                        l.append(-self.triple_to_int(v, i, j2))
                        self.sat.clauses.append(l)

        for v in range(self.k):
            for j in range(self.k):
                l = []
                for i in range(self.k):
                    l.append(self.triple_to_int(v, i, j))
                self.sat.clauses.append(l)

        for v in range(self.k):
            for j in range(self.k):
                for i1 in range(0, self.k - 1):
                    for i2 in range(i1 + 1, self.k):
                        l = []
                        l.append(-self.triple_to_int(v, i1, j))
                        l.append(-self.triple_to_int(v, i2, j))
                        self.sat.clauses.append(l)
    
    def add_fixed_value_clauses(self):
        for i in range(self.k):
            for j in range(self.k):
                if (self.initial[i][j] != '*'):
                    self.sat.clauses.append([self.triple_to_int(self.initial[i][j], i, j)])
                
    def add_sudoku_clauses(self):# only when k is a square number
        r = int(self.k**0.5)
        for a in range(r):
            for b in range(r):
                for v in range(self.k):
                    l = []
                    for i in range(r):
                        for j in range(r):
                            l.append(self.triple_to_int(v, a * r + i,  b * r + j))
                    self.sat.clauses.append(l)

        for a in range(r):
            for b in range(r):
                for v in range(self.k):
                    for i1 in range(r):
                        for j1 in range(r):
                            for j2 in range(j1 + 1, r):
                                self.sat.clauses.append([-self.triple_to_int(v, a * r + i1, b * r + j1), -self.triple_to_int(v, a * r + i1, b * r + j2)])
                            for i2 in range(i1 + 1, r):
                                for j2 in range(r):
                                    self.sat.clauses.append([-self.triple_to_int(v, a * r + i1, b * r + j1), -self.triple_to_int(v, a * r + i2, b * r + j2)])
                                         
    def solve(self):
        if self.sudoku_mode:
            self.add_sudoku_clauses()
        self.add_fixed_value_clauses()
        self.build_generic_clauses()
        self.final[:] = self.initial[:]
        self.sat.walk_sat(4 * len(self.sat.values) * len(self.sat.values))
        for i in range(1, len(self.sat.values)):
            if self.sat.values[i] == True:
                l = self.int_to_triple(i)
                self.final[l[1]][l[2]] = l[0]
         
    ##################################################
    # DISPLAY METHODS
    ##################################################      
     
    def display_before_solving(self):
        print("Initial configuration:")
        for r in range(self.k): 
            print("[{0}]".format(', '.join(map(str, self.initial[r]))))
        print("")    

    def display_after_solving(self):
        if(len(self.final)==0):
            print("Not yet solved")
            return
        print("Solved configuration:")
        for r in range(self.k):    
            print("[{0}]".format(', '.join(map(str, self.final[r]))))
        print("")     
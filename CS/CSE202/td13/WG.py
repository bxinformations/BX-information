# -*- coding: utf-8 -*-

import random
import math
from UF import *

class WG:
    def __init__(self, L): # L is the list of edges
        L.sort(key= lambda e: e[2])
        self.edges=L
        self.adj={}
        for x in L:
            if x[0] not in self.adj:
                self.adj[x[0]]={x[1]:x[2]}
            else:
                self.adj[x[0]][x[1]]=x[2]
            if x[1] not in self.adj:
                self.adj[x[1]]={x[0]:x[2]}
            else:
                self.adj[x[1]][x[0]]=x[2]

    # QUESTION 1	
    def min_cycle_aux(self,w,L,S): 
        if (len(S) == 0):
            if L[0] in self.adj[L[len(L) - 1]]:
                return (w + self.adj[L[len(L) - 1]][L[0]], L + [L[0]])
            else:
                return (91281271279, L)
        W = 91281271278
        Cyc = []
        for v in self.adj[L[len(L) - 1]]:
            if v in S:
                S.remove(v)
                W_now, Cyc_now = self.min_cycle_aux(w + self.adj[L[(len(L) - 1)]][v], L + [v], S)
                S.add(v)
                if W_now < W:
                    W = W_now
                    Cyc[:] = Cyc_now[:]
        return (W, Cyc)
  
    # QUESTION 2
    def min_cycle(self): 
        S = set()
        for v in self.adj:
            S.add(v)
        W = 91281271278
        Cyc = []
        for v in self.adj:
            S.remove(v)
            W_now, Cyc_now = self.min_cycle_aux(0, [v], S)
            S.add(v)
            return (W_now, Cyc_now)

    # QUESTION 3
    # Proof. Since a path connecting all vertices of S is a special kind of spanning tree on S, and w_S given us the cost of T_S, so by the definition we have 
    #        w_S given us the smallest cost of path connected all vertices of S. And W is the smallest circle connected all the vertices and each vertices only appear onece
    #        So the W's correspond path will contain four parts first is L, and second is a path connect L[0] and a vertice in S, third is a path connect L[-1] and a vertice in S
    #        and the fourth part is a path connected all the vertices in S and every vertices only appear once. So W can be written as w + w_1 + w_2 + w_3.
    #        By definition w_1 \geq w_{start}, w_2 \geq w_{end}, w_3 \geq w_S. So W \geq w + w_{start} + w_{end} + w_S.
    # Qed.
    
    # QUESTION 4
    def lower_bound(self,w,L,S): # returns low(L), with w the cost of L, and S the set of vertices not in L
        w_start = 89898989898
        w_end = 281738921739
        for v in self.adj[L[0]]:
            if v in S:
                w_start = min(w_start, self.adj[L[0]][v])
        for v in self.adj[L[-1]]:
            if v in S:
                w_end = min(w_end, self.adj[L[-1]][v])
        w_S = self.weight_min_tree(S)
        return w + w_start + w_end + w_S
  
    # QUESTION 5
    def min_cycle_aux_using_bound(self,bestsofar,w,L,S): 
        if (len(S) == 0):
            if L[0] in self.adj[L[-1]]:
                return (w + self.adj[L[-1]][L[0]], L + [L[0]])
            else:
                return (math.inf, [])
        low = self.lower_bound(w, L, S)
        if low >= bestsofar:
            return (math.inf, [])
        W = bestsofar
        Cyc = []
        for v in self.adj[L[-1]]:
            if v in S:
                S.remove(v)
                W_now, Cyc_now = self.min_cycle_aux_using_bound(W, w + self.adj[L[-1]][v], L + [v], S)
                S.add(v)
                if W_now < W:
                    W = W_now
                    Cyc[:] = Cyc_now[:]
        return (W, Cyc)
   
    def min_cycle_using_bound(self): 
        S = set()
        for v in self.adj:
            S.add(v)
        W = 91281271278
        Cyc = []
        for v in self.adj:
            S.remove(v)
            W_now, Cyc_now = self.min_cycle_aux_using_bound(W, 0, [v], S)
            S.add(v)
            if W_now < W:
                W = W_now
                Cyc[:] = Cyc_now[:]
            return (W_now, Cyc_now)
  
#################################################################
## Auxiliary methods
#################################################################

    def weight_min_tree(self,S): # mincost among all trees whose spanned vertices are those in S
        if len(S)==1: return 0
        if len(S)==2:
            L=list(S)
            if L[0] in self.adj[L[1]]: return self.adj[L[0]][L[1]]
            else: return math.inf    
        uf=UF(S)
        nr_components=len(S) 
        weight=0
        for e in self.edges:
            if e[0] in S and e[1] in S:
                if uf.find(e[0])!=uf.find(e[1]):
                    weight=weight+e[2]
                    uf.union(e[0],e[1]) 
                    nr_components=nr_components-1
                    if nr_components==1:
                        return weight        
        return math.inf
     
    def induce_by_subset(self,S): # reduces self.adj to keep only the edges with both ends in S 
        new_adj={}
        for x in self.adj:
            for y in self.adj[x]:
                if x in S and y in S:
                    if x not in new_adj:
                        new_adj[x]={y:self.adj[x][y]}
                    else:
                        new_adj[x][y]=self.adj[x][y]
                    if y not in new_adj:
                        new_adj[y]={x:self.adj[y][x]}
                    else:
                        new_adj[y][x]=self.adj[y][x]    
        self.adj=new_adj          

    def display(self):
        print("Graph has "+str(len(self.adj))+" vertices")
        print()   
        for x,y in self.adj.items():
            print("Neighbours of "+str(x)+":")	
            for t,u in y.items(): 
                print(str(t)+" with weight "+str(u))
            print()      # -*- coding: utf-8 -*-


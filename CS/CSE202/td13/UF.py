# -*- coding: utf-8 -*-
class UF:
    def __init__(self,S):
        self.id={}
        for x in S:
            self.id[x]=x
        self.rank ={}
        for x in S:
            self.rank[x]=1  
        
    def union(self, p, q):
        r1 = self.find(p)
        r2 = self.find(q)
        
        if r1 == r2:
            return
                    
        if self.rank[r2] > self.rank[r1]:
            self.id[r1] = r2
        else:
            self.id[r2] = r1
            
        if self.rank[r2] == self.rank[r1]:
            self.rank[r1] += 1
        
    def find(self, p):
        if self.id[p] == p:
            return p
        # path compression
        self.id[p] = self.find(self.id[p])
        return self.id[p]
        


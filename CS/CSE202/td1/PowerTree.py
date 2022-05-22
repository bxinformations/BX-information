# -*- coding: utf-8 -*-

class PowerTree:
    def __init__(self): 
        self.layers=[[1]]
        self.parent={1:1}
    
    def draw_tree(self):
        for i in range(len(self.layers)):	
            print("layer",i)
            for j in self.layers[i]: print(j,"->",self.parent[j])  
  	
    def path_from_root(self,k):
        if not (k in self.parent.keys()):
            return -1
        if (k == 1):
            return [1]
        if (self.parent[k] != 1):
            return self.path_from_root(self.parent[k]) + [k]
        else:
            return [1, k]
    
    def add_layer(self):
        k = len(self.layers)
        k -= 1
        self.layers.append([])
        for i in self.layers[k]:
            l = self.path_from_root(i)
            for j in l:
                if  (j + i) in self.parent.keys():
                    continue
                self.layers[k + 1].append(j + i)
                self.parent[j + i] = i
  
  
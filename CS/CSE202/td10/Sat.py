# -*- coding: utf-8 -*-
import random

class Sat:
      
  def __init__(self,n,L):
    self.nr_var=n   # variables are x1,...,xn
    self.clauses=L
    self.values=[True for i in range(n+1)]    
    self.fixed={}
    
  def is_clause_satisfied(self,c):
    f = False
    for i in c:
      if i < 0:
        f = f or (not self.values[-i])
      else:
        f = f or self.values[i]
    return f
        
  def satisfied(self):
    f = True
    for c in self.clauses:
      f = f and (self.is_clause_satisfied(c))
    return f
         
  def initialize(self):
    for i in range(1, self.nr_var + 1):
      self.values[i] = (random.randint(0, 1) == 1)
      
  def walk_sat(self,N):
    self.clauses.sort(key = len)
    cnt = 1
    self.initialize()
    while (self.satisfied() == False):
      if (cnt % N == 0):
        self.initialize()
      for c in self.clauses:
        if (not self.is_clause_satisfied(c)):
          p = random.randint(0, len(c) - 1)
          if (c[p] < 0):
            self.values[-c[p]] = (not self.values[-c[p]])
          else:
            self.values[c[p]] = (not self.values[c[p]])
          break
   
  ##################################################
  # PROPAGATION METHODS
  ##################################################  
   
  def fix_values_from_1clauses(self):
    # TO COMPLETE
    pass
        
  def simplify_clause(self,c):
    res=[]
    for x in c:
        if not abs(x) in self.fixed:
            res.append(x)
        else:
            if (x>0 and self.values[x]) or (x<0 and not self.values[-x]):
                return -1
    return res   

  def simplify_clauses(self):
    res=[]    
    for c in self.clauses:  
       cp=self.simplify_clause(c)
       if not cp==-1:
           res.append(cp)
    return res        
    
  def simplify_formula_by_propagation(self):                                  
     # TO COMPLETE
     pass  

  ##################################################
  # DISPLAY METHODS
  ##################################################                      
                                  
  def clause_to_string(self,c):
     res="" 
     for i in range(0,len(c)):
         if i==0: res="("
         else: res=res+" ∨ "
         if(c[i]>0): res=res+"x"+str(c[i])
         else: res=res+"¬x"+str(-c[i])
     return res+")"
     
  def display_statistics(self):
      print("Number of clauses: "+str(len(self.clauses)))
      print("Number of non-fixed variables: "+str(self.nr_var-len(self.fixed)))
      print("")

  def display_formula(self):
     L=self.clauses
     res=self.clause_to_string(L[0])
     for i in range(1,len(L)):     
         res=res+" ∧ "+self.clause_to_string(L[i])   
     print(res)    
              
  def display_values(self):
     res="" 
     for i in range(1,self.nr_var+1):
         res=res+"x"+str(i)+"="+str(self.values[i])+" "
     print(res)    

            
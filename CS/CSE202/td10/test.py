# -*- coding: utf-8 -*-
from Sat import *
from LatinSquarePuzzle import *

def test_satisfied():
  sat=Sat(4,[[-1,2,-4],[2,3]])
  sat.values=[0,True,False,True,True]
  print("Assignment x1=True, x2=False, x3=True, x4=True")
  print("Test on clause ¬x1 ∨ x2 ∨ ¬x4 ")
  if not sat.is_clause_satisfied([-1,2,-4]):
      print("Success!")
  else: print("Error, expects False")    
  print("Test on clause x2 ∨ x3 ")
  if sat.is_clause_satisfied([2,3]):
      print("Success!")
  else: print("Error, expects True") 
  print("Test on conjonction of the two clauses:")
  if not sat.satisfied():
    print("Success!")
  else:
    print("Error, expects False")
    
def test_walk_sat():
  sat=Sat(8,[[-1, -5], [-3, -7], [-2, -6], [-4, -8], [-1, -2], [-3, -4], [-5, -6], [-7, -8], [-1, -3], [-5, -7], [-2, -4], [-6, -8], [1, 2], [3, 4], [5, 6], [7, 8], [1, 5], [3, 7], [2, 6], [4, 8], [1, 3], [5, 7], [2, 4], [6, 8]])
  N=8*8**2  
  sat.walk_sat(N)
  print("Test on a formula with 8 variables and 24 clauses")
  if sat.satisfied():
      print("Success!")
  else:
      print("Error: does not satisfy all clauses")
  print("")    
    
  sat=Sat(27,[[19], [17], [6], [-1, -10], [-1, -19], [-10, -19], [-4, -13], [-4, -22], [-13, -22], [-7, -16], [-7, -25], [-16, -25], [-2, -11], [-2, -20], [-11, -20], [-5, -14], [-5, -23], [-14, -23], [-8, -17], [-8, -26], [-17, -26], [-3, -12], [-3, -21], [-12, -21], [-6, -15], [-6, -24], [-15, -24], [-9, -18], [-9, -27], [-18, -27], [-1, -2], [-1, -3], [-2, -3], [-4, -5], [-4, -6], [-5, -6], [-7, -8], [-7, -9], [-8, -9], [-10, -11], [-10, -12], [-11, -12], [-13, -14], [-13, -15], [-14, -15], [-16, -17], [-16, -18], [-17, -18], [-19, -20], [-19, -21], [-20, -21], [-22, -23], [-22, -24], [-23, -24], [-25, -26], [-25, -27], [-26, -27], [-1, -4], [-1, -7], [-4, -7], [-10, -13], [-10, -16], [-13, -16], [-19, -22], [-19, -25], [-22, -25], [-2, -5], [-2, -8], [-5, -8], [-11, -14], [-11, -17], [-14, -17], [-20, -23], [-20, -26], [-23, -26], [-3, -6], [-3, -9], [-6, -9], [-12, -15], [-12, -18], [-15, -18], [-21, -24], [-21, -27], [-24, -27], [1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12], [13, 14, 15], [16, 17, 18], [19, 20, 21], [22, 23, 24], [25, 26, 27], [1, 10, 19], [4, 13, 22], [7, 16, 25], [2, 11, 20], [5, 14, 23], [8, 17, 26], [3, 12, 21], [6, 15, 24], [9, 18, 27], [1, 4, 7], [10, 13, 16], [19, 22, 25], [2, 5, 8], [11, 14, 17], [20, 23, 26], [3, 6, 9], [12, 15, 18], [21, 24, 27]])
  N=4*27**2
  sat.walk_sat(N)
  print("Test on a formula with 27 variables and 111 clauses")
  if sat.satisfied():
      print("Success!")
  else:
      print("Error: does not satisfy all clauses")
   
#  l=LatinSquarePuzzle(3,[['*',2,'*'],['*','*',1],[0,'*','*']])
#  l.build_generic_clauses()
#  l.add_fixed_value_clauses()  
#  print(l.sat.clauses)

def test_build_clauses():
  l=LatinSquarePuzzle(2,[['*','*'],['*','*']])
  l.build_generic_clauses()
  m=l.sat.clauses
  #print(m)
  for x in m:
      x.sort()
  m.sort()
  expected=[[-8, -7], [-8, -6], [-8, -4], [-7, -5], [-7, -3], [-6, -5], [-6, -2], [-5, -1], [-4, -3], [-4, -2], [-3, -1], [-2, -1], [1, 2], [1, 3], [1, 5], [2, 4], [2, 6], [3, 4], [3, 7], [4, 8], [5, 6], [5, 7], [6, 8], [7, 8]]
  if(m==expected):
      print("Success!")
  else:
      print("Error, expects")
      print(expected)
      print("but list of clauses is")
      print(m)
      
def test_1clauses():
  l=LatinSquarePuzzle(4,[['*',1,'*',2],['*','*',3,1],[0,'*','*','*'],[1,'*',2,'*']])
  l.add_fixed_value_clauses()
  m=l.sat.clauses
  m.sort()
  expected=[[6], [15], [28], [30], [33], [50], [59]]
  if(m==expected):
      print("Success!")
  else:
      print("Error, expects")
      print(expected)
      print("but list of clauses is")
      print(m)      

def test_sudoku_clauses():
  l=LatinSquarePuzzle(4,[['*','*','*','*'],['*','*','*','*'],['*','*','*','*'],['*','*','*','*']])
  l.add_sudoku_clauses()
  m=l.sat.clauses
  #print(m)
  for x in m:
      x.sort()
  m.sort()
  expected=[[-64, -60], [-64, -48], [-64, -44], [-63, -59], [-63, -47], [-63, -43], [-62, -58], [-62, -46], [-62, -42], [-61, -57], [-61, -45], [-61, -41], [-60, -48], [-60, -44], [-59, -47], [-59, -43], [-58, -46], [-58, -42], [-57, -45], [-57, -41], [-56, -52], [-56, -40], [-56, -36], [-55, -51], [-55, -39], [-55, -35], [-54, -50], [-54, -38], [-54, -34], [-53, -49], [-53, -37], [-53, -33], [-52, -40], [-52, -36], [-51, -39], [-51, -35], [-50, -38], [-50, -34], [-49, -37], [-49, -33], [-48, -44], [-47, -43], [-46, -42], [-45, -41], [-40, -36], [-39, -35], [-38, -34], [-37, -33], [-32, -28], [-32, -16], [-32, -12], [-31, -27], [-31, -15], [-31, -11], [-30, -26], [-30, -14], [-30, -10], [-29, -25], [-29, -13], [-29, -9], [-28, -16], [-28, -12], [-27, -15], [-27, -11], [-26, -14], [-26, -10], [-25, -13], [-25, -9], [-24, -20], [-24, -8], [-24, -4], [-23, -19], [-23, -7], [-23, -3], [-22, -18], [-22, -6], [-22, -2], [-21, -17], [-21, -5], [-21, -1], [-20, -8], [-20, -4], [-19, -7], [-19, -3], [-18, -6], [-18, -2], [-17, -5], [-17, -1], [-16, -12], [-15, -11], [-14, -10], [-13, -9], [-8, -4], [-7, -3], [-6, -2], [-5, -1], [1, 5, 17, 21], [2, 6, 18, 22], [3, 7, 19, 23], [4, 8, 20, 24], [9, 13, 25, 29], [10, 14, 26, 30], [11, 15, 27, 31], [12, 16, 28, 32], [33, 37, 49, 53], [34, 38, 50, 54], [35, 39, 51, 55], [36, 40, 52, 56], [41, 45, 57, 61], [42, 46, 58, 62], [43, 47, 59, 63], [44, 48, 60, 64]]
  if(m==expected):
      print("Success!")
  else:
      print("Error, expects")
      print(expected)
      print("but list of clauses is")
      print(m)

def display_and_solve(l):
  l.display_before_solving()
  l.solve()
  l.display_after_solving()

def test_solve():
 l=LatinSquarePuzzle(3,[['*',2,'*'],['*','*',2],['*','*','*']])
 display_and_solve(l)   
 
 print("******************************************")
    
 k=4
 l=LatinSquarePuzzle(k,[['*' for _ in range(k)] for __ in range(k)])
 display_and_solve(l)

 print("*****************************************")

 l=LatinSquarePuzzle(4,[['*',1,'*',2],['*','*',3,1],[0,'*','*','*'],[1,'*',2,'*']])
 display_and_solve(l)
 
 print("*****************************************")

 l=LatinSquarePuzzle(4,[['*',1,'*','*'],['*','*',3,'*'],[0,'*','*','*'],['*','*',2,'*']])
 display_and_solve(l)

def test_4sudoku():
 l=LatinSquarePuzzle(4,[['*',1,'*','*'],['*','*',3,'*'],[0,'*','*','*'],['*','*',2,'*']])
 l.sudoku_mode=True
 display_and_solve(l) 

def test_propagate():
    l=LatinSquarePuzzle(4,[[3,1,'*',2],[2,'*',3,'*'],[0,'*','*',3],[1,'*','*','*']])
    l.build_generic_clauses()
    l.add_fixed_value_clauses()
    l.sat.simplify_formula_by_propagation()    
    if(len(l.sat.fixed)<64):
        print("Error: propagation should fix all variables")
    else:
        if(l.sat.satisfied):
            print("Success!")
        else:
            print("Error: propagated all variables, but did not reach a solution")
            
            

def test_sudoku_easy():
 easy = [[5, 8, '*', '*', '*', 1, '*', '*', '*'],
            ['*', 3, '*', '*', 6, '*', '*', 7, '*'],
            [0, '*', '*', 3, 2, '*', 1, '*', 6],
            ['*', '*', '*', '*', '*', '*', '*', 5, '*'],
            [3, '*', 0, '*', '*', '*', 2, '*', 1],
            ['*', 5, '*', '*', '*', '*', '*', '*', '*'],
            [6, '*', 2, '*', 5, 7, '*', '*', 8],
            ['*', 4, '*', '*', 8, '*', '*', 1, '*'],
            ['*', '*', '*', 1, '*', '*', '*', 6, 5]]

 l=LatinSquarePuzzle(9,easy)
 l.sudoku_mode=True
 display_and_solve(l)
 
def test_sudoku_medium(): 
 medium = [[2,'*',5,'*',4,'*','*',6,'*'],
            ['*',3,'*','*',6,'*','*',2,'*'],
            [8,'*',6,1,'*','*','*',4,'*'],
            ['*','*','*','*',5,'*','*',1,'*'],
            ['*',1,'*',3,8,'*',4,'*','*'],
            [5,'*','*','*','*',6,'*','*',8],
            ['*',6,'*',5,'*','*',1,'*',0],
            [0,'*',8,'*','*','*',3,'*','*'],
            [1,'*','*','*',3,'*','*','*',2]]

 l=LatinSquarePuzzle(9,medium)
 l.sudoku_mode=True
 display_and_solve(l)

def test_sudoku_hard():
 hard = [['*', 2, '*', '*', '*', '*', '*', 3, '*'],
            ['*', '*', '*', 6, '*', 1, '*', '*', '*'],
            ['*', 6, 8, 2, '*', '*', '*', '*', 5],
            ['*', '*', 0, '*', '*', 8, 3, '*', '*'],
            ['*', 4, 6, '*', '*', '*', 7, 5, '*'],
            ['*', '*', 1, 3, '*', '*', 4, '*', '*'],
            [0, '*', '*', '*', '*', 7, 5, 1, '*'],
            ['*', '*', '*', 1, 0, 4, '*', '*', '*'],
            ['*', 1, '*', '*', '*', '*', '*', 0, '*']]
 l=LatinSquarePuzzle(9,hard)
 l.sudoku_mode=True
 display_and_solve(l)


''' CALLS TO THE TEST FUNCTIONS '''

#test_satisfied()
#test_walk_sat()
#test_build_clauses()
#test_1clauses()
#test_sudoku_clauses()
#test_solve()
#test_4sudoku()
# test_propagate()
# test_sudoku_easy()
# test_sudoku_medium()
# test_sudoku_hard()




                 

# -*- coding: utf-8 -*-

import random
import time

from WG import *

L_tutorial=[['A','B',2],['B','C',1],['C','D',1],['D','E',2],['E','F',1],['F','G',2],['G','H',1],['H','A',1],['A','F',1],['B','E',1],['D','G',1],['C','H',5]]

def test1():
    global L_tutorial
    wg=WG(L_tutorial)
    print("Tests on the graph of Figure 1")
    print()
    print("Test with input L=['A','F','G']")
    res=wg.min_cycle_aux(3,['A','F','G'],set(['B','C','D','E','H']))
    expected=(14, ['A', 'F', 'G', 'D', 'E', 'B', 'C', 'H', 'A'])
    if res==expected: print("Success!")
    else:
        print("Error, expected")
        print(expected)
        print("but output is")
        print(res)
    print()
    print("Test with input L=['H','C']")
    res=wg.min_cycle_aux(5,['H','C'],set(['A','B','G','D','E','F']))
    expected1=(14, ['H', 'C', 'D', 'G', 'F', 'E', 'B', 'A', 'H'])
    expected2=(14, ['H', 'C', 'B', 'E', 'D', 'G', 'F', 'A', 'H'])
    if res==expected1 or res==expected2: print("Success!")
    else:
        print("Error, expected")
        print(expected1," or ",expected2)
        print("but output is")
        print(res)
    print()
    print("Test with input L=['G']")
    res=wg.min_cycle_aux(0,['G'],set(['A','B','C','D','E','F','H']))
    Lis=['G', 'H', 'A', 'F', 'E', 'B', 'C', 'D', 'G']
    expected=(8, ['G', 'H', 'A', 'F', 'E', 'B', 'C', 'D', 'G'])
    Lis2=Lis[:]; Lis2.reverse(); expected2=(8,Lis2)
    if res==expected or res==expected2: print("Success!")
    else:
        print("Error, expected")
        print(expected)
        print("but output is")
        print(res)
    print()    
    print("Test with input L=['A','B','C','D','E','F','G','H']")
    res=wg.min_cycle_aux(10,['A','B','C','D','E','F','G','H'],set([]))
    expected=(11, ['A','B','C','D','E','F','G','H','A'])
    if res==expected: print("Success!")
    else:
        print("Error, expected")
        print(expected)
        print("but output is")
        print(res)
 
def equal_cycles(L1,L2):
    if len(L1)!=len(L2): return False
    if len(L1)==1: return L1[0]==L2[0]    
    L1.pop(); L2.pop()
    if not L1[0] in L2: return False
    i=L2.index(L1[0])
    if L1==L2[i:]+L2[:i]: return True
    L2.reverse()    
    i=L2.index(L1[0])
    if L1==L2[i:]+L2[:i]: return True
    return False

def test2(): 
    global L_tutorial
    wg=WG(L_tutorial)
    print("Test on the graph of Figure 1")
    res=wg.min_cycle()
    expected=['G', 'H', 'A', 'F', 'E', 'B', 'C', 'D', 'G']
    if res[0]==8 and equal_cycles(res[1],expected):
        print("Success!")
    else:    
        print("Error, expected")
        print((8,expected))
        print("but output is")
        print(res)

def test4():
    global L_tutorial
    wg=WG(L_tutorial)
    print("Test on the graph of Figure 1")
    print()
    print("Test with input L=['F']")
    res=wg.lower_bound(0,['F'],set(['A', 'B', 'C', 'D', 'E', 'G', 'H']))
    if res==8: print("Success!")
    else:
        print("Error, expected 8, but output is "+str(res))
    print()
    print("Test with input L=['D','E']")
    res=wg.lower_bound(2,['D','E'],set(['A', 'B', 'C', 'F', 'G', 'H']))
    if res==10: print("Success!")
    else:
        print("Error, expected 10, but output is "+str(res))
    print()
    print("Test with input L=['A','B','E','F']")
    res=wg.lower_bound(4,['A','B','E','F'],set(['C', 'D', 'G', 'H']))
    if res==10: print("Success!")
    else:
        print("Error, expected 10, but output is "+str(res))
    print()
    print("Test with input L=['A','B','E','D']")
    res=wg.lower_bound(5,['A','B','E','D'],set(['C', 'F', 'G', 'H']))
    if res==15: print("Success!")
    else:
        print("Error, expected 15, but output is "+str(res))
    print()
         
def random_graph(n):
    L=[]
    for i in range(n):
        for j in range(i+1,n):
            L.append([i,j,random.random()])
    return WG(L)

def test_random_graph(n):    
    wg=random_graph(n)
    start = time.perf_counter()
    print(wg.min_cycle())
    elapsed = (time.perf_counter() - start)
    print("min_cycle took time "+str(elapsed))
    print()
    start = time.perf_counter()
    print(wg.min_cycle_using_bound())
    elapsed = (time.perf_counter() - start)
    print("min_cycle_using_bound took time "+str(elapsed))  
  
 
def read_cities():
    cities=["Barcelona","Belgrade","Berlin","Brussels","Bucharest","Budapest","Copenhagen","Dublin","Hamburg","Istanbul","Kiev","London","Madrid","Milan","Moscow","Munich","Paris","Prague","Rome","Saint Petersbourg", "Sofia", "Stockholm", "Vienna", "Warsaw"]
    distances=[]
    with open("cities.txt") as f:
        content = f.readlines()
        distances = [x.strip().split() for x in content]
        #print(content)
    list_edges=[]    
    for j in range(1,len(cities)):
        for i in range(j):
            list_edges.append([cities[i],cities[j],int(distances[i][j])])       
    return WG(list_edges)   

def display_min_tour_cities(cities_in_tour): 
    wg=read_cities()
    wg.induce_by_subset(cities_in_tour)
    #wg.display()
    print(str(len(cities_in_tour))+" cities in the tour")
    print(wg.min_cycle_using_bound())
    print()  

def test_trip():
    cities_in_tour1=set(["Belgrade","Berlin","Budapest","Copenhagen","Brussels", "Stockholm", "Vienna"])     
    display_min_tour_cities(cities_in_tour1)
    cities_in_tour2=set(["Barcelona","Belgrade","Berlin","Brussels","Hamburg","Paris","Istanbul","Kiev","London","Moscow","Prague","Rome","Saint Petersbourg", "Sofia", "Stockholm", "Vienna"])     
    display_min_tour_cities(cities_in_tour2) 
    
    
''' CALLS TO TEST FUNCTIONS '''    
    
test1()    
test2()
test4()
n=9; test_random_graph(n)
test_trip()


    

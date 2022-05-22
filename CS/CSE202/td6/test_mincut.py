import mincut as sol

def two_clique_graph(n): 
# produces graph on 2*n vertices with two cliques (a first clique for vertices of label in [1..n], a second clique
# for vertices with labels in [n+1..2n]). The two cliques are connected by n-2 edges, and thus the mincut has
# size n-2, and is uniquely realized as the cut into S=[1..n] and complement(S)=[n+1..2n] 
  list_edges=[]
  for i in range(1,n+1):
    for j in range(i+1,n+1):
      list_edges.append([i,j,1])
      list_edges.append([2*n-i+1,2*n-j+1,1])
  for i in range(1,n-1):
      list_edges.append([i,2*n-i+1,1])
  return [2*n,list_edges]


L_tutorial=[8,[['a','c',1],['a','g',1],['a','d',1],['a','b',1],['g','c',2],['g','d',1],['d','c',1],['b','e',1],['b','h',1],['b','f',2],['f','h',1],['e','h',1],['e','f',1],['e','d',1],['g','h',1]]]
L_tutorial_mincut = [3, ['a', 'c', 'd', 'g']]

#m=MultiGraph(L_tutorial)
#print(m.cutsize(11))

def compare_cut(graph, cut, cutsol):
    # list vertices
    vert=set()
    for [a,b,_] in graph[1]:
        vert.add(a)
        vert.add(b)
    assert cut[0] == cutsol[0], "your cut has the wrong size {} instead of {}".format(cut[0], cutsol[0])
    #print("{} vs {} vs {}".format(set(cut[1]), set(cutsol[1]), vert-set(cutsol[1])))
    scut = set(cut[1])
    scutsol = set(cutsol[1])
    assert scut == scutsol or scut == vert-scutsol, "your cut vertices are incorrect"

def test_mincut_brute(): 
  print("Testing mincut_brute")  
  s=sol.mincut_brute(sol.MultiGraph(L_tutorial))
  if s is None:
         print("Skipping mincut_brut, unimplemented")
         return  
  #print("example from the tutorial")
  print("If no error message then everything is correct")
  compare_cut(L_tutorial, s, L_tutorial_mincut)
  for n in range(3,9):
     #print("two-clique graph with "+str(2*n)+" vertices")
     G = two_clique_graph(n)
     compare_cut(G, sol.mincut_brute(sol.MultiGraph(G)), [n-2, [i for i in range(1,n+1)]])


def same_graph(g1, g2):
    assert g1.adj == g2.adj, "the graph is not correct"
    assert g1.deg == g2.deg, "the degrees are not correct"

def test_contract():
  print()   
  print("Testing contract")  
  Lt=[5,[['a','b',1],['a','c',1],['a','e',1],['b','c',1],['b','e',2],['c','e',1],['c','d',1]]]
  Lt_cont_eb=[4,[['a','e',2],['a','c',1],['c','e',2],['c','d',1]]]
  Lt_cont_cd=[4,[['a','b',1],['a','c',1],['a','e',1],['b','c',1],['b','e',2],['c','e',1]]]
  m=sol.MultiGraph(Lt)
  m2=sol.MultiGraph(Lt_cont_eb)
  print("Multigraph middle of Figure 3")
  #m.display()
  print()
  print("Contraction of {e,b}. Should obtain the multigraph on the left of Figure 3 (if no error message then result is correct)")
  m.contract('e','b')
  same_graph(m, m2)
  #m.display()
  m=sol.MultiGraph(Lt)
  m2=sol.MultiGraph(Lt_cont_cd)
  print()
  print("Contraction of {c,d}. Should obtain the multigraph on the right of Figure 3 (if no error message then result is correct)")
  m.contract('c','d')
  #m.display()
  same_graph(m, m2)


  
def test_random_element():
  print()   
  print("Testing random element")  
  data={'a':2, 'b':1, 'c':4}
  na=0; nb=0; nc=0 
  ntrials=50000
  for _ in range(ntrials):
    u = sol.random_element(data)
    if u == 'a':
      na += 1
    elif u == 'b':
      nb += 1
    elif u == 'c':
      nc += 1
    else:
      print("Error: your function returned a wrong value {}".format(u))
  print("Frequency of a (should be close to 0.28): "+str(na/ntrials))
  print("Frequency of b (should be close to 0.14): "+str(nb/ntrials))
  print("Frequency of c (should be close to 0.57): "+str(nc/ntrials))
  err = 0.05
  assert abs(na/ntrials-2/7)<=err and abs(nb/ntrials-1/7)<=err and abs(nc/ntrials-4/7)<=err, \
      "your code does not have the right probabilities"


def test_random_edge():
  print()   
  print("Testing random edge")   
  L_graph=[3,[['a','b',1],['a','c',2],['b','c',3]]]
  nab=0; nac=0; nbc=0 
  ntrials=50000
  for _ in range(ntrials):
   m=sol.MultiGraph(L_graph)
   [u,v]=m.random_edge()
   if (u=='a' and v=='b') or  (u=='b' and v=='a'):
     nab=nab+1
   if (u=='a' and v=='c') or  (u=='c' and v=='a'):
     nac=nac+1
   if (u=='b' and v=='c') or  (u=='c' and v=='b'):
     nbc=nbc+1
  print("Frequency of {a,b} (should be close to 0.166): "+str(nab/ntrials))
  print("Frequency of {a,c} (should be close to 0.333): "+str(nac/ntrials))
  print("Frequency of {b,c} (should be close to 0.5): "+str(nbc/ntrials))
  err = 0.05
  assert abs(nab/ntrials-1/6)<=err and abs(nac/ntrials-1/3)<=err and abs(nbc/ntrials-1/2)<=err, \
      "your code does not have the right probabilities"



def test_random_cut():
  print()   
  print("Testing random cut")    
  L_graph=[4,[['a','b',1],['b','c',1],['c','d',1],['a','d',2]]]
  nab=0; nac=0; nad=0; na=0; nb=0; nc=0; nd=0 
  ntrials=10000
  for _ in range(ntrials):
     m=sol.MultiGraph(L_graph)
     [c,L]=sol.random_cut(m)
     if c==3 and (len(L)==1 and 'a' in L) or (len(L)==3 and 'a' not in L):
       na=na+1
     elif c==2 and ((len(L)==1 and 'b' in L) or (len(L)==3 and 'b' not in L)):
       nb=nb+1
     elif c==2 and ((len(L)==1 and 'c' in L) or (len(L)==3 and 'c' not in L)):
       nc=nc+1
     elif c==3 and ((len(L)==1 and 'd' in L) or (len(L)==3 and 'd' not in L)):
       nd=nd+1
     elif c==3 and len(L)==2 and (('a' in L and 'b' in L) or ('c' in L and 'd' in L)):
       nab=nab+1
     elif c==2 and len(L)==2 and (('a' in L and 'd' in L) or ('b' in L and 'c' in L)):
       nad=nad+1
     else:
       nac=nac+1
  print("Frequency of {a} U {b,c,d} (should be close to 0.1): "+str(na/ntrials))
  print("Frequency of {b} U {a,c,d} (should be close to 0.233): "+str(nb/ntrials))
  print("Frequency of {c} U {a,c,d} (should be close to 0.233): "+str(nc/ntrials))
  print("Frequency of {d} U {a,b,c} (should be close to 0.1): "+str(nd/ntrials))
  print("Frequency of {a,b} U {c,d} (should be close to 0.1): "+str(nab/ntrials))
  print("Frequency of {a,c} U {b,d} (should be 0): "+str(nac/ntrials))
  print("Frequency of {a,d} U {b,c} (should be close to 0.233): "+str(nad/ntrials)) 



def test_mincut_randomized():
  print()   
  print("Testing randomized mincut")     
  for i in range(15,22): 
    print("two-clique graph with "+str(2*i)+" vertices")
    L=two_clique_graph(i)
    print(sol.mincut_karger(L,0.1))
    
#test_mincut_brute()  
#test_contract()
#test_random_element() 
#test_random_edge()
test_random_cut() 
#test_mincut_randomized()


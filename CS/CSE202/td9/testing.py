# -*- coding: utf-8 -*-

from uf import UF_base, Rank_UF
import random
import math 

### union find

def test_random_uf():
    print("Random testing of union find (run several times tu be sure to pass the test)")
      
    N = 100
    print("N =", N)  
    uf1 = UF_base(N)
    uf2 = Rank_UF(N)
    
    pairs_union = []
    for _ in range(10):
        i,j = random.randint(0,N-1),random.randint(0,N-1)
        uf1.union(i,j)
        uf2.union(i,j)
        pairs_union.append([i,j])
        
    for p in range(N-1):
        for q in range(p+1, N):
            if not uf1.is_connected(p,q) and uf2.is_connected(p,q):
                print("Fail")
                print("Sequence of unions: ", pairs_union )

    print("Success")        


def test_rank_uf():
    print('Testing tutorial example with Rank_UF')
    N = 12
    uf = Rank_UF(N)
    
    uf.union(0,3)
    uf.union(6,3)    
    uf.union(9,6)
    
    uf.union(1,4)
    uf.union(7,1)    
    uf.union(10,4)
    
    uf.union(2,5)
    uf.union(8,2)    
    uf.union(11,2)
    
    expA =  [0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2]
    if not uf.A  == expA:
        print('Fail. Expected A=', expA, " gives A=", uf.A)
    
    expR = [2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    if not uf.ranks  == expR:
        print('Fail. Expected rank=', expR, " gives rank=", uf.ranks)
    
    print('Success')
    
### percolation
import perco

def test_get_neighbors():
    print('Testing get_vacant_neighbors')
    N = 6
    G = [[False for _ in range(N)] for _ in range(N)]
    
    G[2][2] = True

    
    R = []
    L = perco.get_vacant_neighbors(G, N, 2, 2)
    if not R == L:
        print('Fail. Expected ', R, ', got ', L)
    
    
    G[3][2] = True
    G[2][3] = True
    G[1][2] = True
    G[2][1] = True
    G[3][3] = True   
    R = [[1,2], [2, 1], [3, 2], [2, 3]]
    L = perco.get_vacant_neighbors(G, N, 2, 2)
    for p in L:
        if not p in R:
            print('Fail. Expected ', R, ', got ', L)
    
    for p in R:
        if not p in L:
            print('Fail. Expected ', R, ', got ', L)
            
    print('Success')



def test_make_vacant():
    print('Testing make_vacant')
    N = 6
    G = [[False for _ in range(N)] for _ in range(N)]
    
    G[2][2] = True
    uf = Rank_UF(N*(N+2))
    
    perco.make_vacant(uf, G, N, 2, 2)
    if not G[2][2]:
        print('Fail. Grid at position (2,2) is not vacant')
        
    perco.make_vacant(uf, G, N, 3, 2)
    if not G[3][2] or not uf.is_connected(perco.pos_to_int(N,2,2),perco.pos_to_int(N,3,2)):
        print('Fail. Grid at position (3,2) is not vacant or not joined to (2,2)')
    
    perco.make_vacant(uf, G, N, 4, 3)
    if not G[4][3] or uf.is_connected(perco.pos_to_int(N,3,2),perco.pos_to_int(N,4,3)):
        print('Fail. Grid at position (4,3) is not vacant or joined to (3,2)')
    print('Success')
    

### Hex

from Hex import Hex

def test_hex_neighbors():
    print('Testing Hex.neighbours')
    
    N = 6
    h = Hex(N)
    
    h.board[2][2] = 1
    h.board[2][3] = 2
    L = h.neighbours(2,2)
    E = []    
    if L != E:
        print('Fail. Expects ', E,', gets ',L)
        
    E = [[1,2], [2,1], [3,2], [1,3], [3,1]]
    for p in E:
        h.board[p[0]][p[1]] = 1
    L = h.neighbours(2,2)
    
    for p in L:
        if p not in E:
            print('Fail. Expects ', E,', gets ',L)
    for p in E:
        if p not in L:
            print('Fail. Expects ', E,', gets ',L)
    print('Success')
   
def test_hex_winning():
    print('Testing Hex.is_game_over')
    
    N = 6
    h = Hex(N)
    
    
    L = [[2,2],[1,2],[2,3],[3,3],[4,3],[5,3],[6,3]]
    for p in L:
        h.board[p[0]][p[1]] = 1
        for n in h.neighbours(p[0],p[1]):
            h.uf.union(h.hex_to_int(p[0],p[1]), h.hex_to_int(n[0],n[1]))

    if not h.is_game_over():
        print('Fail. Player 1 has won')
        h.print_board()
        
    h = Hex(N)
    
    h.player= 2
    for p in L:
        h.board[p[0]][p[1]] = 1
        for n in h.neighbours(p[0],p[1]):
            h.uf.union(h.hex_to_int(p[0],p[1]), h.hex_to_int(n[0],n[1]))


    if h.is_game_over():
        print('Fail. Player 2 has not won')
        h.print_board()
        
    print('Success')
    
def test_erdos():
    x = range(10, 500, 10)
    y = []
    for i in x:
        y.append(Erdos_Renyi(i))
        
    plt.plot(x,y, color='red', label='experimental')
    plt.plot(x,[(math.log(N))*(N-1)/2 for N in x], color='blue', label='model')
    
    plt.legend(loc='upper left')
    
    plt.xlabel('N')
    plt.ylabel('#Edges')
    plt.title('Erdos-Renyi: number of edges for being connected')
    plt.show()

    
''' CALLS TO THE TESTING FUNCTIONS '''

''' Union Find '''
#test_random_uf()
#test_rank_uf()   

''' Erdos-Renyi '''
#test_erdos()

''' Percolation '''
#test_get_neighbors() 
#test_make_vacant()   
#N = 500; print("N =",N,", ratio to percolate =", perco.ratio_to_percolate(N))     

''' Hex game '''
#test_hex_neighbors()
#test_hex_winning()



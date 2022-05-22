# -*- coding: utf-8 -*-

from uf import Rank_UF
import random
import matplotlib.pyplot as plt
import math 
 
 
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
    
def Erdos_Renyi(N):
    m = Rank_UF(N)
    i = 0
    num = 0
    while (m.count > 1):
        a = random.randint(0, N - 1)
        b = random.randint(0, N - 1)
        num += 1
        if (m.is_connected(a, b)):
            continue
        i += 1
        m.union(a, b)
    return num

test_erdos()
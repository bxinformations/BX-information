# -*- coding: utf-8 -*-

from uf import Rank_UF

import random


def draw_grid(grid, N):
    for ii in range(N):
        i = ii+1
        for j in range(N):
            if grid[i][j] == 0:
                print('X', end='')
            else:
                print(' ', end='')
        print()

def pos_to_int(N, i, j):
    return N*i+j


def get_vacant_neighbors(G,N,i,j):
    ans = []
    d = [(-1, 0), (0, -1), (1, 0), (0, 1)]
    for (x, y) in d:
            if (i + x <= N + 1) and (i + x >= 0) and (j + y < N) and (j + y >= 0):
                if (G[i + x][j + y]):
                    ans.append([i + x, j + y])
    return ans

def make_vacant(UF, G, N, i, j):
    nei = get_vacant_neighbors(G, N, i, j)
    G[i][j] = True
    for p in nei:
        UF.union(pos_to_int(N, i, j), pos_to_int(N, p[0], p[1]))
     

def ratio_to_percolate(N):    
    G = [[False for _ in range(N)] for _ in range(N + 2)]
    print(len(G[0]))
    for i in range(N):
        G[0][i] = True
        G[N + 1][i] = True
    
    NF = Rank_UF(N * (N + 2))
    for i in range(1, N):
        NF.union(pos_to_int(N, 0, i), pos_to_int(N, 0, i - 1))
        NF.union(pos_to_int(N, N + 1, i), pos_to_int(N, N + 1, i - 1))

    while (not NF.is_connected(pos_to_int(N, 0, 0), pos_to_int(N, N + 1, 0))):
        x = random.randint(1, N)
        y = random.randint(0, N - 1)
        if not (G[x][y]):
            make_vacant(NF, G, N, x, y)

    sum = 0
    for i in range(1, N + 1):
        for j in range(N):
            if (G[i][j]):
                sum += 1
    return sum * 1.0 / (N * N)
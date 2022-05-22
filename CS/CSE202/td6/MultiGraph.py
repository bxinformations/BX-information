# -*- coding: utf-8 -*-
import random

def random_element(dict):
    tot = 0
    L = []
    for x in dict:
        tot += dict[x]
        for i in range(dict[x]):
            L.append(x)
    return L[random.randint(0, tot - 1)]


class MultiGraph:
    def __init__(self, L):
        self.adj = {}
        self.deg = {}
        self.n = L[0]
        for x in L[1]:
            if x[0] not in self.adj:
                self.adj[x[0]] = {x[1]: x[2]}
                self.deg[x[0]] = x[2]
            else:
                self.adj[x[0]][x[1]] = x[2]
                self.deg[x[0]] += x[2]
            if x[1] not in self.adj:
                self.adj[x[1]] = {x[0]: x[2]}
                self.deg[x[1]] = x[2]
            else:
                self.adj[x[1]][x[0]] = x[2]
                self.deg[x[1]] += x[2]

    def cutsize(self, i):  # i is an integer between 1 and 2^n-2, with n the number of vertices
        L = []
        for x in self.adj:
            if (i & 1 == 1):
                L.append(x)
            i >>= 1
        c = 0
        for x in L:
            for y in self.adj[x]:
                if y in L:
                    continue
                c += self.adj[x][y]
        return [c, L]

    def random_vertex(self):
        return random_element(self.deg)

    def random_edge(self):
        i = self.random_vertex()
        j = random_element(self.adj[i])
        return (i, j)

    def contract(self, i, j):  # contracts edge i,j (i absorbs j)
        self.n -= 1
        self.deg[i] = self.deg[i] + self.deg[j] - 2 * self.adj[i][j]
        for y in self.adj[j]:
            if (y == i):
                continue
            if (y not in self.adj[i]):
                self.adj[i][y] = self.adj[j][y]
            else:
                self.adj[i][y] += self.adj[j][y]
        del self.adj[j]
        del self.deg[j]
        del self.adj[i][j]
        for x in self.adj:
            if (j in self.adj[x]):
                if (i not in self.adj[x]):
                    self.adj[x][i] = self.adj[x][j]
                    del self.adj[x][j]
                else:
                    self.adj[x][i] += self.adj[x][j]
                    del self.adj[x][j]

    def display(self):
        for x, y in self.adj.items():
            print("Neighbours of "+str(x) +
                  ", which has degree "+str(self.deg[x]))
            for t, u in y.items():
                print(str(t)+" with multiplicity "+str(u))

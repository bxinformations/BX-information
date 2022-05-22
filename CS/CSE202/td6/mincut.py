from MultiGraph import *

def mincut_brute(m):
    n = ((1 << m.n) - 2) >> 1
    c = 123456789
    ans = []
    for i in range(1, n):
        res = m.cutsize((i << 1) + 1)
        if (c > res[0]):
            c = res[0]
            ans[:] = res[:]
    return ans
  
def random_cut(m):
    n = m.n
    S = {}
    l = ''
    for i in range(n - 2):
        x, y = m.random_edge()
        m.contract(x, y)
        if (x not in S):
            if (y not in S):
              S[x] = [y]
            else:
              S[x]= [y]+S[y]
              del S[y]

        elif y not in S:
            S[x].append(y)
        else:
            S[x] += [y]+S[y]
            del S[y]
    l = next(iter(m.deg))
    if l not in S:
        return [m.deg[l], [l]]
    return [m.deg[l], [l] + S[l]]
    
def mincut_karger(L,e): # e is the wished error bound
    m = MultiGraph(L) 
    n = m.n
    tot = 1
    p = (1 - 2.0 / (n * (n - 1)))
    while (p > e):
        tot += 1
        p *= (1 - 2.0 / (n * (n - 1)))

    c = 123456789
    ans = []
    for i in range(tot):
        res = random_cut(MultiGraph(L))
        if (res[0] < c):
            c = res[0]
            ans[:] = res[:]

    return ans

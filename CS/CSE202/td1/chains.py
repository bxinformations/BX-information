# -*- coding: utf-8 -*-

import math
from PowerTree import *

def bin_pow(x, n):
    res = 1
    while n:
        if (n & 1):
            res = res * x
        x *= x
        n >>= 1
    return res

def cost_bin_pow(n):
    n = int(n)
    if (n == 0) or (n == 1):
        return 0
    if (n & 1):
        return 2 + cost_bin_pow((n - 1) / 2)
    return 1 + cost_bin_pow(n / 2)

def smallest_factor(n):
    mx = int(n ** 0.5) + 1
    for i in range(2, mx):
        if (n % i == 0):
            return i
    return -1

def factor_pow(x,n):
    if (n == 0):
        return 1
    if (n == 1):
        return x
    f = smallest_factor(n)
    if (f == -1):
        return x * factor_pow(x, n - 1)
    else:
        return factor_pow(factor_pow(x, f), n / f)

def cost_factor_pow(n):
    if (n == 0) or (n == 1):
        return 0
    f = smallest_factor(n)
    if (f == -1):
        return 1 + cost_factor_pow(n - 1)
    else:
        return cost_factor_pow(f) + cost_factor_pow(n / f)

def get_l(n, m):
    if (n == 1):
        return [1]
    f = smallest_factor(n)
    if (f == -1):
        if ((n - 1) in m.keys()):
            return m[n - 1] + [n]
        res = get_l(n - 1, m)
        m[n] = res + [n]
        return m[n]
    else:
        l1 = []
        l2 = []
        if (f in m.keys()):
            l1 = m[f]
        else:
            l1 = get_l(f, m)
        if ((n / f) in m.keys()):
            l2 = m[n / f]
        else:
            l2 = get_l(n / f, m)
        l2 = l2[1:]
        k = len(l2)
        for i in range(k):
            l2[i] *= f
        m[n] = l1 + l2
        return m[n]

def power_from_chain(x,L):
    m = {}
    k = len(L)
    n = L[k - 1]
    while (n):
        if not (n in m.keys()):
            get_l(n, m)
        if (m[n] == L):
            return bin_pow(x, n)
        n -= 1

## Q8 ##

def power_tree_chain(n):
    T = PowerTree()
    x = T.path_from_root(n)
    while (x == -1):
        T.add_layer()
        x = T.path_from_root(n)
    return x

def power_tree_pow(x,n):
    m = {}
    l = power_tree_chain(n)
    m[1] = x
    c = len(l)
    for i in range(1, c):
        m[l[i]] = m[l[i - 1]] * m[l[i - l[i - 1]]]
    return m[n]
    	   
def cost_power_tree_pow(n):
    l = power_tree_chain(n)
    return len(l) - 1
  
## Q9 ##  
def compare_costs(m):
    print('Cost of bin_pow ' + str(cost_bin_pow(m)))
    print('Cost of factor_pow ' + str(cost_factor_pow(m)))
    print('Cost of power_tree_pow ' + str(cost_power_tree_pow(m)))

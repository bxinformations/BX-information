# -*- coding: utf-8 -*-

from cmath import exp, pi
import numpy as np
import timeit

def printc(L):
    res=[]
    for a in L:
        res.append('{:.4f}'.format(a))
    print(res)
    
def eval_poly(L, x):
    res = 0
    for v in reversed(L):
        res = res * x + v
    return res

def dft_slow(L):
    d = len(L)
    n = 0
    res = 1
    while (res < d):
        n += 1
        res = res << 1
    w = exp(- 2 * pi * 1j / res)
    y = []
    for i in range(res):
        y.append(eval_poly(L, w ** i))
    return y

def sum_list(a, b):
    pass

def fft_sum(a, b):
    N = len(b) * 2
    w = exp(-2 * pi * 1j / N)
    res = 1
    for i in range(N // 2):
        b[i] = res * b[i]
        res = res * w
    c = [0 for _ in range(N)]
    for i in range(N // 2):
        c[i] = a[i] + b[i]
        c[i + N // 2] = a[i] - b[i]
    return c

def recursive_fft(L):
    if (len(L) == 1):
        return [eval_poly(L, 1)]
    
    N = len(L)
    e = L[0::2]
    o = L[1::2]
    ey = recursive_fft(e)
    eo = recursive_fft(o)
    return fft_sum(ey, eo)

def com():
    print(timeit.timeit(lambda:dft_slow([1.3 -2, 6, 3]), number = 1))
    print(timeit.timeit(lambda:recursive_fft([1.3 -2, 6, 3]), number = 1))
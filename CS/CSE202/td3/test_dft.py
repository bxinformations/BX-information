# -*- coding: utf-8 -*-
import dft as dft
from cmath import exp, pi
import numpy as np
import timeit
import random

grading_mode = False

def test1():
    if dft.eval_poly([1,4,3],6) is None:
        print("skipping eval_poly (unimplemeneted)")
        assert not grading_mode
        return
    tmp = dft.eval_poly([1,4,3],6)
    if tmp != 133:
        print("Your function eval_poly() returned the wrong result ({} instead of 133) on [1,2,3,4],6".format(tmp))
        assert not grading_mode
    tmp = dft.eval_poly([2,3,1,5,6,4],5+1j)
    res = (11047 + 14767j)
    if tmp != res:
        print("Your function eval_poly() returned the wrong result ({} instead of {}) on [2,3,1,5,6,4],5+1j".format(tmp, res))
        assert not grading_mode
    print("eval_poly done.")

def err_dist(L, L2):
    return max([abs(L[i]-L2[i]) for i in range(len(L))])

def check_dft(name, fn):
    for L in [[1,4,6,8],[1,3,2,-5],[10,32,45,-7,18,-42,48,74]]:
        tmp = fn(L)
        res = np.fft.fft(L)
        err = err_dist(tmp, res)
        if err >= 0.0001:
            print("Your function {}() returned the wrong result ({} instead of {}) on {}: error is {}".format(name, tmp, res, L, err))
            assert not grading_mode
        

def test2():
    if dft.dft_slow([1,2,3,4]) is None:
        print("skipping dft_slow (unimplemeneted)")
        assert not grading_mode
        return
    check_dft("dft_slow", dft.dft_slow)
    print("dft_slow done.")

def test3():
    tmp = dft.fft_sum([4, 5], [42, 43])
    if tmp is None:
        print("skipping fft_sum (unimplemeneted)")
        assert not grading_mode
        return
    res = [(46+0j), (5-43j), (-38), (5+43j)]
    err = err_dist(res, tmp)
    if err >= 0.0001:
        print("Your function fft_sum() returned the wrong result ({} instead of {}) on [4, 5], [42, 43], error is {}".format(tmp, res, err))
        assert not grading_mode
    print("fft_sum done.")

def test4():
    if dft.recursive_fft([1,2,3,4]) is None:
        print("skipping recursive_fft (unimplemeneted)")
        assert not grading_mode
        return
    check_dft("recursive_fft", dft.recursive_fft)
    print("recursive_fft done.")

test1()
test2()
test3()
test4()

def print_running_times(n):
    print("runtimes: dft_slow | fft_recursive | numpy")
    for i in range(5,n):
        N=2**i
        L=[random.uniform(0.0,256.0) for j in range(N)]
        if(i>12): 
            elapsed0=-1    
        else:
            elapsed0 = timeit.timeit(lambda: dft.dft_slow(L), number=5)
        elapsed1 = timeit.timeit(lambda: dft.recursive_fft(L), number=5)
        elapsed2 = timeit.timeit(lambda: np.fft.fft(L), number=5)
        res="N="+str(N)+": "
        if elapsed0==-1:
            res+="large"
        else:
            res+=('{:10.2e}'.format(elapsed0))		
        for a in [elapsed1,elapsed2]:
            res+=" | "+('{:10.2e}'.format(a))	 
        print(res)	
 
# uncomment to run
print_running_times(16)
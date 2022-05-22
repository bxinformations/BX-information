# -*- coding: utf-8 -*-

from gmpy2 import mpz
import timeit
from math import log

#########################################################     
###   CODE TO COMPUTE FACTORIAL (BINARY SPLITTING)    ###
#########################################################

def facto_bin_aux(ns, ne):
    if ns == ne:
        return mpz(ns)
    m = (ns+ne)//2
    return facto_bin_aux(ns, m) * facto_bin_aux(m+1, ne)
    
def facto_bin(n):
    if n == 0 or n == 1:
        return mpz(1) 
    return facto_bin_aux(1,n)


# QUESTION 1

def numer_iter(n):
    if n == 0:
        return 1
    return n * numer_iter(n - 1) + 1
    
# QUESTION 2    
    
def prod_mat(K,L):
    res = [[0, 0], [0, 0]]
    for i in range(2):
        for j in range(2):
            for k in range(2):
                res[i][j] += mpz(K[i][k]) * mpz(L[k][j])
    return res
    
def numer_bin(n):
    res = [[1, 0], [0, 1]]
    for i in range(n - 1):
        A = [[i + 3, -1], [i + 2, 0]]
        res = prod_mat(A, res)
    return int(mpz(res[0][0]) * mpz(2) + mpz(res[0][1]) * mpz(1))
    
# QUESTION 3    
    
def exp_digits(k):
    res = log(2)
    mx = k * log(10) + log(3)
    n = 1
    while (res <= mx):
        n += 1
        res += log(n + 1)
    
    res = mpz(numer_bin(n))
    for i in range(k):
        res = res * mpz(10)
    d = facto_bin(n)
    return res // mpz(d)

#########################################################     
###       CODE TO COMPARE RUNNING TIMES (GIVEN)       ###
#########################################################  

def compare_times_numer():
  print("runtimes: numer iterative | numer binary")
  n=18
  for i in range(3,n):
    N=2**i
    elapsed0 = timeit.timeit(lambda:numer_iter(N), number=1)
    elapsed1 = timeit.timeit(lambda:numer_bin(N), number=1)
    res="N="+str(N)+": "
    res+=('{:10.2e}'.format(elapsed0))		
    res+=" | "+('{:10.2e}'.format(elapsed1))	
    print(res)
        

# uncomment to runtimes
#compare_times_numer()

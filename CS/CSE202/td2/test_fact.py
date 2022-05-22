# -*- coding: utf-8 -*-
import fact as fact
import timeit

grading_mode = False

def check_fact(fn, name):
    if fn(1) is None:
        print("skipping {} (unimplemeneted)".format(name))
        assert not grading_mode
        return
    for (x,y) in [(0,1), (1,1), (2,2), (3,6), (20, 2432902008176640000)]:
        tmp = fn(x)
        if tmp != y:
            print("your factorial of {} returned {} instead of {}".format(x, tmp, y))
        assert not grading_mode
    print("{} done".format(name))

def test6():
    check_fact(fact.factor_iter, "factor_iter")

def test7():
    check_fact(fact.factor_bin, "factor_bin")

test6()
test7()

#########################################################     
###       CODE TO COMPARE RUNNING TIMES (GIVEN)       ###
#########################################################  

def compare_naive_bin():
  print("runtimes: iterative | binary")
  n=17
  for i in range(3,n):
    N=2**i
    elapsed0 = timeit.timeit(lambda:fact.factor_iter(N), number=1)
    elapsed1 = timeit.timeit(lambda:fact.factor_bin(N), number=1)
    res="N="+str(N)+": "
    res+=('{:10.2e}'.format(elapsed0))		
    res+=" | "+('{:10.2e}'.format(elapsed1))	
    print(res)

def compare_bin_mpz():
  print("runtimes: binary without mpz | binary with mpz")
  n=21
  for i in range(3,n):
    N=2**i
    elapsed0 = timeit.timeit(lambda:fact.factor_bin(N), number=1)
    elapsed1 = timeit.timeit(lambda:fact.factor_bin_faster(N), number=1)
    res="N="+str(N)+": "
    res+=('{:10.2e}'.format(elapsed0))		
    res+=" | "+('{:10.2e}'.format(elapsed1))	
    print(res)   
   
######################################### 

# uncomment to run
#compare_naive_bin()

# uncomment to run
#compare_bin_mpz()

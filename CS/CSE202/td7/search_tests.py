# -*- coding: utf-8 -*-

from search import *
import random
import string
import time
import matplotlib.pyplot as plt

def test_basic(algo):
    T = "aaaaaabaacaaabaaaacaaaabaaacaaabaaaaaaabaab"
    P = "aab"
    m = algo(T,P)
    if m != [4, 11, 21, 29, 37, 40]:
        print("wrong matches:",m)
        assert False, "wrong matches"
            
def test1():
    print("Testing string_match")
    P = 'abra'
    T = 'abacadabra'
    pos = string_match(T,P)
    if(pos is None):
        print("String_match unimplemented")
        return
    print("Testing T= ", T, ", P = ", P, ": 1 match at position 6.")
    if  len(pos) == 1 and 6 in pos:
        print("Success.")
    else:
        print("Failure. Returned ",pos)
        
    P = 'abra'
    T = 'abracadabra'
    print("Testing T= ", T, ", P = ", P, ": 2 match at positions 0 and 7.")
    pos = string_match(T,P)    
    if  len(pos) == 2 and 0 in pos and 7 in pos:
        print("Success.")
    else:
        print("Failure. Returned ",pos) 


def test4():
    print("Testing karp_rabin_sum")
    P = 'abra'
    T = 'abacaaabra'
    print("Testing T= ", T, ", P = ", P, ": 1 match at position 6, 1 false hit.")
    pos,fh = karp_rabin_sum(T,P)    
    if  len(pos) == 1 and 6 in pos and fh == 1:
        print("Success.")
    else:
        print("Failure. Returned ", pos, fh)
        
    P = 'abra'
    T = 'abracaaabra'
    print("Testing T= ", T, ", P = ", P, ": 2 match at positions 0 and 7, 1 false hit.")
    pos,fh = karp_rabin_sum(T,P)    
    if  len(pos) == 2 and 0 in pos and 7 in pos and fh == 1:
        print("Success.")
    else:
        print("Failure. Returned ", pos, fh)
        
def time_krsum_worst(nmax, maxseq):
    nrun = 5
    x = range(maxseq, nmax+1,2)
    ynaive = []
    ykrsum = []
    ykr = []
    
    ms2 = maxseq//2
    usekr = False
    
    P = ('a'*(maxseq-1))+'b' #('a'*ms2) + ('b'*ms2)
    for i in x:    
        T = ('a'*(maxseq-1)+'b')*(i//maxseq) + 'a'*(i%maxseq)
        
        t = time.time()
        for _ in range(nrun):
            mkrsum,sh = karp_rabin_sum(T, P)
        t = time.time() - t
        ykrsum.append(t) 
        
        #print(i,sh)
        t = time.time()
        for _ in range(nrun):
            mnaive = string_match(T, P)
        t = time.time() - t
        ynaive.append(t)
     
        if not mnaive == mkrsum:
            print("error",i, mnaive, mkrsum)
            
    plt.plot(x,ynaive,color='blue', label='naive')
    plt.plot(x, ykrsum, color='red', label='karp_rabin_sum')
    if usekr:
        plt.plot(x,ykr, color='green', label='karp_rabin')
    plt.legend(loc='upper left')
    plt.title("running time")
    plt.show()        

def test_krsum_worst_case():
    time_krsum_worst(1000,100)
            
def test5():
    print("Testing karp_rabin_mod")
    Q = 181    
    print("Prime:", Q)
    P = 'abra'
    T = 'abacaaabra'
    print("Testing T= ", T, ", P = ", P, ": 1 match at position 6, 0 false hit.")
    pos,fh = karp_rabin_mod(T,P,Q)    
    if  len(pos) == 1 and 6 in pos and fh == 0:
        print("Success.")
    else:
        print("Failure. Returned ", pos, fh)
        
    P = 'abra'
    T = 'abracaaabra'
    print("Testing T= ", T, ", P = ", P, ": 2 match at positions 0 and 7, 0 false hit.")
    pos,fh = karp_rabin_mod(T,P,Q)    
    if  len(pos) == 2 and 0 in pos and 7 in pos and fh == 0:
        print("Success.")
    else:
        print("Failure. Returned ", pos, fh)

    Q = 7    
    print("Prime:", Q)
    P = 'abra'
    T = 'abacaaabra'
    print("Testing T= ", T, ", P = ", P, ": 1 match at position 6, 1 false hit.")
    pos,fh = karp_rabin_mod(T,P,Q)    
    if  len(pos) == 1 and 6 in pos and fh == 1:
        print("Success.")
    else:
        print("Failure. Returned ", pos, fh)
        
    P = 'abra'
    T = 'abracaaabra'
    print("Testing T= ", T, ", P = ", P, ": 2 match at positions 0 and 7, 1 false hit.")
    pos,fh = karp_rabin_mod(T,P,Q)    
    if  len(pos) == 2 and 0 in pos and 7 in pos and fh == 1:
        print("Success.")
    else:
        print("Failure. Returned ", pos, fh)


def test_krmod_primes():
    # random 8,16,32 and 64 bit primes
    q8 = 181
    q16 = 14107
    q32 = 1674011477
    q64 = 5229701266470813371
    
    maxm = 1000
    
    x = range(2, maxm+1,10)

    y8 = []
    y16 = []    
    y32 = []
    y64 = []    

    fm8 = []
    fm16 = []    
    fm32 = []
    fm64 = []
    
    for m in x:
        n = m*100
        T=''
        for _ in range(n):
            T+=random.choice(string.ascii_lowercase)
        P=''
        for _ in range(m):
            P+=random.choice(string.ascii_lowercase)
            
        t = time.time()
        match,fmatch = karp_rabin_mod(T, P, q8)
        t = time.time() - t
        y8.append(t)
        fm8.append(fmatch)
        
                
        t = time.time()
        match,fmatch = karp_rabin_mod(T, P, q16)
        t = time.time() - t
        y16.append(t)
        fm16.append(fmatch)
        
        t = time.time()
        match,fmatch = karp_rabin_mod(T, P, q32)
        t = time.time() - t
        y32.append(t)
        fm32.append(fmatch)
        
        t = time.time()
        match,fmatch = karp_rabin_mod(T, P, q64)
        t = time.time() - t
        y64.append(t)        
        fm64.append(fmatch)

    plt.plot(x,y8,color='blue',label='8 bit')
    plt.plot(x,y16,color='red',label='16 bit')
    plt.plot(x,y32,color='green',label='32 bit')
    plt.plot(x,y64,color='magenta',label='64 bit')    
    plt.title("running time")
    plt.legend(loc="upper left")
    plt.show()
    
    plt.plot(x,fm8,color='blue',label='8 bit')
    plt.plot(x,fm16,color='red',label='16 bit')
    plt.plot(x,fm32,color='green',label='32 bit')
    plt.plot(x,fm64,color='magenta',label='64 bit')    
    plt.title("number of false matches")
    plt.legend(loc="upper left")
    plt.show()
    
##### CALLS TO TEST FUNCTIONS #########
    
#test1()    
#test4()    
#test_krsum_worst_case()
#test5()    
test_krmod_primes()

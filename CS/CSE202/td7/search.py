# -*- coding: utf-8 -*-

### For comparing strings

def string_compare(P,S):
    for j in range(len(P)):
        if not P[j] == S[j]:
            return False
    return True
    
    
### naive string matcher    
def string_match(T, P):
    ans = []
    for i in range(len(T) - len(P) + 1):
        if (string_compare(T[i : i + len(P)], P)):
            ans.append(i)
    return ans

### number of characters
base = 256

### karp_rabin_sum 

def hash_string_sum(S):
    res = 0
    for i in range(len(S)):
        res += ord(S[i])
    return res

def hash_string_sum_update(h, Ti, Tim):
    return h - ord(Ti) + ord(Tim)
    
def karp_rabin_sum(T,P):
    h = hash_string_sum(T[0:len(P) - 1])
    ha = hash_string_sum(P)
    ans = []
    f = 0
    ti = chr(0)
    tim = T[len(P) - 1]
    for i in range(len(T) - len(P) + 1):
        h = hash_string_sum_update(h, ti, tim)
        if (h == ha):
            if (string_match(T[i:i+len(P)], P)):
                ans.append(i)
            else:
                f += 1
        if (i != len(T) - len(P)): 
            ti = T[i]
            tim = T[i + len(P)]
    return (ans, f)
    
### karp_rabin_mod
    
def hash_string_mod(S, q):
    res = 0
    m = 1
    for i in range(len(S)):
        res = res + (m * ord(S[len(S) - i - 1]) % q)
        m = m * base % q
    return res%q
    
def hash_string_mod_update(h, q, Ti, Tim, dm):
    return (base * (h - (dm % q) * ord(Ti)) + ord(Tim)) % q
    
def karp_rabin_mod(T, P, q):
     dm = (1 << (8 * (len(P) - 1)))
    h = hash_string_mod(T[0:len(P) - 1], q)
    ha = hash_string_mod(P, q)
    ans = []
    f = 0
    ti = chr(0)
    tim = T[len(P) - 1]
    for i in range(len(T) - len(P) + 1):
        h = hash_string_mod_update(h, q, ti, tim, dm)
        if (h == ha):
            if (string_match(T[i:i+len(P)], P)):
                ans.append(i)
            else:
                f += 1
        if (i != len(T) - len(P)): 
            ti = T[i]
            tim = T[i + len(P)]
    return (ans, f)

#with the larger bit, we get less number of false matches, but we takes more time to run.
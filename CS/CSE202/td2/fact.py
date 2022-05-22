# Q6
from gmpy2 import mpz 

def factor_iter(n):
    res = 1
    for i in range(1, n + 1):
        res *= i
    return res

# Q7

def factor_bin_aux(ns, ne):
    if (ns == ne):
        return ne
    if (ns > ne):
        return 1
    mid = (ns + ne) >> 1
    return factor_bin_aux(ns, mid) * factor_bin_aux(mid + 1, ne)
            
    
def factor_bin(n):
    if (n == 0):
        return 1
    return factor_bin_aux(1, n)
    
# Q8  
    
def factor_bin_faster_aux(ns, ne):
    if (ns == ne):
        return ns
    mid = (ns + ne) >> 1
    x = mpz(factor_bin_aux(ns, mid))
    y = mpz(factor_bin_aux(mid + 1, ne))
    return  x * y
    
def factor_bin_faster(n):
    return factor_bin_faster_aux(1, n)

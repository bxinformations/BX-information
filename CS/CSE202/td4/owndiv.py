# -*- coding: utf-8 -*-

def dual_for_tests(x):    
    n=x.bit_length()
    return 2**(2*n-1)//x # NOTE: we use division here for debugging purposes only!
    
## QUESTION 4    
    
def dual_dac(x):
    t = x
    n = 0
    while (t != 0):
        n += 1
        t >>= 1
    
    if (x == (1 << (n - 1))):
        return 1 << n

    n2 = (n + 1) >> 1
    x2 = (x) >> (n >> 1)
    dx2 = dual_dac(x2)
    d = ((dx2 << 1) * ((1 << (n + n2)) - x * dx2)) >> (n2 << 1)

    r = (1 << (2 * n - 1)) - d * x
    t = 0
    if (r < 0):
        while (r < 0):
            r += x
            t += 1
    elif (r >= x):
        while (r >= x):
            r -= x
            t += 1
    return d + t

## QUESTION 5      

def division(A,B):
    t = A
    p = 0
    while (t != 0):
        p += 1
        t >>= 1
    t = B
    q = 0
    while (t != 0):
        q += 1
        t >>= 1

    k = max(0, 1 + p - 2 * q)
    C = dual_dac((1 << k) * B)
    Q = (A * C) >> (2 * q + k - 1)
    if (A < B * (Q + 1)):
        return Q
    return Q + 1

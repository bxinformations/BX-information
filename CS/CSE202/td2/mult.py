# -*- coding: utf-8 -*-

# Q1

def poly_mult(P,Q):
    n = len(P)
    m = len(Q)
    R = [0 for _ in range(n + m - 1)]
    for i in range(n):
        for j in range(m):
            R[i + j] += P[i] * Q[j]
    return R

def cost_mult(n): 
    return 2 * n * n - n - n + 1

# Q2

def poly_add(P,Q):
    n = len(P)
    m = len(Q)
    while (n < m):
        n += 1
        P.append(0)
    while (n > m):
        m += 1
        Q.append(0)
    R = [0 for _ in range(n)]
    for i in range(n):
        R[i] = P[i] + Q[i]
    return R
         
def neg(P):
    n = len(P)
    for i in range(n):
        P[i] = -P[i]
    return P
   
def shift(P,k):
    K = [0 for _ in range(k)]
    return K + P
  
# Q3  
  
def poly_kara_mult(P,Q):
    n = len(P)
    m = len(Q)
    while (n < m):
        n += 1
        P.append(0)
    while (n > m):
        m += 1
        Q.append(0)
    
    if n <= 2:
        return poly_mult(P, Q)
    
    k = (n + 1) // 2

    P1 = P[:k]
    P2 = P[k:]
    Q1 = Q[:k]
    Q2 = Q[k:]
    H0 = poly_kara_mult(P1, Q1)
    H1 = poly_kara_mult(poly_add(P1, P2), poly_add(Q1, Q2))
    H2 = poly_kara_mult(P2, Q2)
    return poly_add(H0, poly_add(shift(poly_add(H1, neg(poly_add(H0, H2))), k), shift(H2, 2 * k)))
    
def cost_poly_kara_mult(n):
    if (n <= 1):
        return cost_mult(n)
    return 3 * cost_poly_kara_mult((n + 1) // 2) + 4 * n

# Q4 

def cost_poly_tc3_mult(n):
    if n == 1:
        return 1
    if n == 2:
        return 3
    return 5 * cost_poly_tc3_mult((n + 2) // 3) + 30 * n

# Q5 hybrid
   
def poly_switch_mult(d,P,Q):
    n = len(P)
    m = len(Q)
    while (n < m):
        n += 1
        P.append(0)
    while (n > m):
        m += 1
        Q.append(0)
    
    if n <= d:
        return poly_mult(P, Q)
    
    k = (n + 1) // 2

    P1 = P[:k]
    P2 = P[k:]
    Q1 = Q[:k]
    Q2 = Q[k:]
    H0 = poly_switch_mult(d, P1, Q1)
    H1 = poly_switch_mult(d, poly_add(P1, P2), poly_add(Q1, Q2))
    H2 = poly_switch_mult(d, P2, Q2)
    return poly_add(H0, poly_add(shift(poly_add(H1, neg(poly_add(H0, H2))), k), shift(H2, 2 * k)))

def cost_switch_mult(d,n):
    if (n <= d):
        return cost_mult(n)
    return 3 * cost_switch_mult(d, (n + 1) // 2) + 4 * n

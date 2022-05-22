# -*- coding: utf-8 -*-

### Question 1 ###
def peak_naive(L):
    n = len(L)
    if (n == 1):
        return 0
    for i in range(n):
        if (i == 0):
            if (L[i] >= L[i + 1]):
                return i
        elif (i == n - 1):
            if (L[i] >= L[i - 1]):
                return i
        else:
            if (L[i] >= L[i - 1]) and (L[i] >= L[i + 1]):
                return i
    
### Question 2 ###   
def peak(L):    
    l = 0
    r = len(L) - 1
    if (r - l < 2):
        if (r - l < 1):
            return 0
        else:
            if (L[l] >= L[r]):
                return l
            else:
                return r
    mid = (l + r) >> 1
    if (L[mid] >= L[mid - 1]):
        if (mid + 1 <= r):
            if (L[mid] >= L[mid + 1]):
                return mid
            else:
                return peak(L[mid: r + 1]) + mid
        else:
            return mid
    else:
        if (mid - 2 >= l):
            if (L[mid - 1] >= L[mid - 2]):
                return mid - 1
            else:
                return peak(L[l: mid]) + l
        else:
            return mid - 1
    
def peak_aux(L,p,q):    
    # TO IMPLEMENT
    pass    
    
### Question 3 ###
def is_peak(M,i,j):
    I = len(M)
    J = len(M[0])
    if (i - 1 >= 0):
        if (M[i][j] < M[i - 1][j]):
            return False
    if (j - 1 >= 0):
        if (M[i][j] < M[i][j - 1]):
            return False
    if (i + 1 < I):
        if (M[i][j] < M[i + 1][j]):
            return False
    if (j + 1 < J):
        if (M[i][j] < M[i][j + 1]):
            return False
    return True
    
### Question 4 ###
def peak2d_naive(M):
    I = len(M)
    J = len(M[0])
    for i in range(I):
        for j in range(J):
            if (is_peak(M, i, j)):
                return (i, j)

### Question 5 ###
def pivot(M,p,q,r,s):    
    if (q - p == 1):
        Apq = [p]
    else:
        Apq = [p, ((p + q) >> 1) - 1, q - 1]
    if (s - r == 1):
        Ars = [r]
    else:
        Ars = [r, ((r + s) >> 1) - 1, s - 1]
    (p1, p2) = (0, 0)
    mx = -123434566

    for i in Apq:
        for j in range(r, s):
            if (M[i][j] > mx):
                mx = M[i][j]
                p1 = i
                p2 = j
    
    for j in Ars:
        for i in range(p, q):
            if (M[i][j] > mx):
                mx = M[i][j]
                p1 = i
                p2 = j
    return (p1, p2)
    
### Question 6 ###
def peak2d(M):    
    I = len(M)
    J = len(M[0])
    return peak2d_aux(M, 0, I, 0, J)
  
def peak2d_aux(M,p,q,r,s):
    (x, y) = pivot(M, p, q, r, s)
    if (q - p <= 4) or (s - r) <= 4:
        return (x, y)
    elif (is_peak(M, x, y)):
        return (x, y)
    m = (r + s) >> 1
    l = (p + q) >> 1
    if (x < l) and (y < m):
        return peak2d_aux(M, p, l, r, m)
    if (x < l) and (y >= m):
        return peak2d_aux(M, p, l, m, s)
    if (x >= l) and (y < m):
        return peak2d_aux(M, l, q, r, m)
    if (x >= l) and (y >= m):
        return peak2d_aux(M, l, q, m, s)

# The time complexity should be C(n) = C(n / 2) + O(n), So C(n) = O(n)

def C(s):
    return (8*(1<<s)+2*(1<<s)*(1<<s))/(3**(s))
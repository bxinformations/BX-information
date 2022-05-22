# -*- coding: utf-8 -*-

def str_compare(a, b):
    N = min(len(a),len(b))
    for i in range(N):
        if a[i] < b[i]:
            return -1
        elif a[i] > b[i]:
            return 1

    return len(a)-len(b)

def str_compare_m(a,b, m):
    if len(a) >= m and len(b) >= m:
        # len(a) >= m and len(b) >= m
        return str_compare(a[:m], b[:m])
    else:
        # len(a) < m or len(b) > m
        return str_compare(a,b)

def longest_common_prefix(a, b):
    N = min(len(a),len(b))
    for i in range(N):
        if a[i] != b[i]:
            return i
    return N

    
class suffix_array:
    def __init__(self, t):
        self.T = t
        self.N = len(t)
        self.suffId = [i for i in range(self.N)]
        self.suffId.sort(key = self.key)
        #For the time complexity, we have C(n) = C(n/2) + O(N^2) by the master theorem we have the time complexity is O(N^2 log N)
        #For the space, we have S(n) = S(n / 2) + O(N) this is because every time we compare two string, we need O(N) space to store the string, by the master theorem, we have the spcae is O(N log N)

    def key(self, i):
        return self.T[i:]
        
    def suffix(self, i):
        return self.T[self.suffId[i]:]
        
    def findL(self, S):
        l = -1
        r = self.N
        m = len(S)

        while (l + 1 < r):
            mid = (l + r) >> 1
            if (str_compare_m(self.suffix(mid), S, m) >= 0):
                r = mid
            else:
                l = mid

        return r
        
    def findR(self,S):
        l = -1
        r = self.N
        m = len(S)

        while (l + 1 < r):
            mid = (l + r) >> 1
            if (str_compare_m(S, self.suffix(mid), m) >= 0):
                l = mid
            else:
                r = mid

        return r

    # for both findL and findR, the time complexity satisfy C(n) = C(n / 2) + m, so by the master theorem we have the time complexity is O(mlog N)
    
    def findLR(self,S):
        return (self.findL(S),self.findR(S))

def KWIC(sa, S, c = 15):
    l, r = sa.findLR(S)
    ans = []
    for i in range(l, r):
        ans.append(sa.T[max(0, sa.suffId[i] - c): min(sa.N, sa.suffId[i] + len(S) + c)])
    return ans
    
def longest_repeated_substring(sa):
    ans = ''
    max_len = 0
    for i in range(sa.N - 1):
        now_len = longest_common_prefix(sa.suffix(i), sa.suffix(i + 1))
        if (now_len > max_len):
            max_len = now_len
            ans = sa.suffix(i)[:max_len]
    return ans

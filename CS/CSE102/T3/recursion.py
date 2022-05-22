#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar  5 08:07:00 2020

@author: peixin.you
"""

def recurrence(n):
    if (n == 0 or n == 1):
        return 1
    return 3 * recurrence(n - 1) + recurrence(n - 2)

def binom(n, m):
    if (m == 0 or m == n):
        return 1
    return binom(n - 1, m) + binom(n - 1, m - 1)

def not_angry_e(n, p):
    if (n <= 1):
        return 1
    if (p == 1):
        return not_angry_e(n - 1, 0)
    return not_angry_e(n - 1, 0) + not_angry_e(n - 1, 1)

def not_angry(n):
    if (n == 0):
        return 1
    return not_angry_e(n, 0) + not_angry_e(n, 1)

def not_so_angry_e(n, k, p):
    if (n < p):
        return 0
    if (n == p or n == 1):
        return 1
    if (p == 0):
        return sum([not_so_angry_e(n - 1, k, i) for i in range(k + 1)])
    return not_so_angry_e(n - 1, k, p - 1)

def not_so_angry(k, n):
    return not_so_angry_e(n + 1, k, 0)

def exp(base, d):
    if (d == 0):
        return 1
    if (d == 1):
        return base
    ans = exp(base, d // 2)
    ans *= ans
    if (d & 1 == 0):
        return ans
    return ans * base

def num_calls(d):
    if (d == 0 or d == 1):
        return 1
    return num_calls(d // 2) + 1
   
def permutations(l):
    n = len(l)
    if (n == 1):
        return [l]
    if (n < 1):
        return [[]]
    
    
    result = []
    for i in range(n):
        res_list = l[:i] + l[i + 1:]
        s = l[i]
        per_result = permutations(res_list)
        if (len(per_result) == 1):
            result.append(l[i:i + 1] + per_result[0])
        else:
            result += [[s] + j for j in per_result]
    return result

def Subset(l):
    if (len(l) == 0):
        return [[]]
    result = []
    first_e = l[0]
    rest_l = l[1:]
    for partial_subset in Subset(rest_l):
        result.append(partial_subset)
        next_subset = partial_subset[:] + [first_e]
        result.append(next_subset)
    return result
    
def subsets(S):
    if (S == set()):
        return [set()]
    ans = Subset(list(S))
    return [set(i) for i in ans]

def expen(arr, i):
    n = len(arr)
    result = [[0 for j in range(n - 1)] for k in range(n - 1)]
    for j in range(n - 1):
        for k in range(i):
            result[j][k] = arr[j + 1][k]
    for j in range(n - 1):
        for k in range(n - i - 1):
            result[j][k + i] = arr[j + 1][k + i + 1]
    return result

def find_assignment(prices):
    if (prices == []):
        return (0, [])
    if (len(prices) == 1):
        return (prices[0][0], [0])
    mn = 123123123
    ans = []
    for i in range(len(prices)):
        prices_now = expen(prices, i)
        mn_now, ans_now = find_assignment(prices_now)
        if mn_now + prices[0][i] < mn:
            mn = mn_now + prices[0][i]
            for j in range(len(ans_now)):
                if ans_now[j] >= i:
                    ans_now[j] += 1
            ans = [i] + ans_now
    return (mn, ans)
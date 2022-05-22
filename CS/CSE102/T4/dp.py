#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 12 08:05:47 2020

@author: peixin.you
"""

def not_angry(n):
    f = [[123132, 123123] for i in range(n + 1)]
    f[0] = [1, 0]
    for i in range(1, n + 1):
        f[i][0] = f[i - 1][0] + f[i - 1][1]
        f[i][1] = f[i - 1][0]
    return f[n][0] + f[n][1]

def not_so_angry(k, n):
    f = [[123123123 for i in range(k + 1)] for j in range(n + 1)]
    f[0] = [0 for i in range(k + 1)]
    f[0][0] = 1
    for i in range(1, n + 1):
        f[i][0] = sum(f[i - 1])
        for k in range(1, k + 1):
            f[i][k] = f[i - 1][k - 1]
    return sum(f[n])

def binom(n, k):
    f = []
    n = n + 1
    for i in range(n):
        f.append([])
        for j in range(n):
            f[i].append(0)
    for i in range(n):
        f[i][0] = 1
        f[i][i] = 1
    for i in range(2, n):
        for j in range(1, i):
            f[i][j] = f[i - 1][j - 1] + f[i - 1][j]
    return f[n - 1][k]

f = [123123123 for i in range(1000007)]

def coins(n):
    for i in range(n + 1):
        f[i] = 123123123
    coin = [1, 3, 7, 8]
    f[0] = 0
    for i in range(n):
        for j in range(4):
            if (i + 1 - coin[j] >= 0):
                f[i + 1] = min(f[i + 1], f[i + 1 - coin[j]] + 1)
    return f[n]

def coins_list(n):
    coin = [1, 3, 7, 8]
    coins(n)
    result = []
    while (n > 0):
        for i in range(4):
            if f[n] == f[n - coin[i]] + 1:
                result.append(coin[i])
                n -= coin[i]
                break
    return result

def num_boxes(n):
    for i in range(n + 1):
        f[i] = 123123123
    f[0] = 0
    f[1] = 1
    for i in range(1, n + 1):
        tmp = 1
        while (tmp ** 3 <= i):
            f[i] = min(f[i], f[i - tmp ** 3] + 1)
            tmp += 1
    return f[n]

def find_boxes(n):
    num_boxes(n)
    result = []
    while (n > 0):
        for i in range(n):
            if (i ** 3 > n):
                break
            if f[n] == f[n - (i + 1) ** 3] + 1:
                result.append((i + 1) ** 3)
                n -= (i + 1) ** 3
                break
    return result

def tj_penalty(L, W):
    n = len(W)
    for i in range(n + 1):
        f[i] = 123123123
    cost = [0]
    for i in range(n):
        cost.append(len(W[i]))
    f[0] = 0
    for i in range(1, n + 1):
        for j in range(i - 1, -1, -1):
            if (L >= sum(cost[j + 1: i + 1]) + i - j - 1):
                f[i] = min(f[i], f[j] + (L - sum(cost[j + 1: i + 1]) - (i - j - 1)) ** 3)
    return f[n]

def tj(L, W):
    n = len(W)
    cost = [0]
    for i in range(n):
        cost.append(len(W[i]))
    result = []
    tj_penalty(L, W)
    while (n > 0):
        for i in range(n - 1, -1, -1):
            if f[n] == f[i] + (L - sum(cost[i + 1: n + 1]) - (n - i - 1)) ** 3:
                result.append(i)
                n = i
                break
    ans = ''
    for i in range(len(W)):
        if (i) in result:
            ans += W[i] + '\n'
        else:
            ans += W[i] + ' '
    return ans
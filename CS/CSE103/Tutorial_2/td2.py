#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 12 13:18:09 2020

@author: peixin.you
"""

from data import *

### Exercise 1 ####

def seq_searchF(arr,item):
    f = None
    for i in range(len(arr)):
        if (arr[i] == item):
            f = i
    return f

def seq_searchW(arr,item):
    f = None
    i = 0
    n = len(arr) - 1
    while (f == None) and (i <= n):
        if (arr[i] == item):
            f = i
        i +=1
    return f

def test_ex1():
    lst1 = [1,3,2,4,5,3,8]
    for i in [1,8,3,5,7]:
        print("For-loop: ", seq_searchF(lst1,i))
        print("While-loop: ", seq_searchW(lst1,i))
    # other testing code here...

def seq_searchFC(arr,item):
    f = None
    c = 0
    for i in range(len(arr)):
        c += 1
        if (arr[i] == item):
            f = i
    return (f, c)

def seq_searchWC(arr,item):
    f = None
    i = 0
    c = 0
    n = len(arr) - 1
    while (f == None) and (i <= n):
        c += 1
        if (arr[i] == item):
            f = i
        i +=1
    return (f, c)

def test_ex2():
    for n in [1, 2, 3]:
        print("For-loop: ", seq_searchFC(unsorted,n))
        print("While-loop: ", seq_searchWC(unsorted,n))

def seq_searchS(arr,item):
    l = 0
    r = len(arr) - 1
    while (l < r):
        mid = (l + r) >> 1
        if (arr[mid] < item):
            l = mid + 1
        else:
            r = mid
    return l

def test_ex3():
    for n in [1, 2, 3]:
        print("Sequential Search: ", seq_searchS(sorted_list,n))
        print("While-loop: ", seq_searchW(sorted_list,n))

def seq_searchSC(arr,item):
    l = 0
    r = len(arr) - 1
    c = 0
    while (l < r):
        mid = (l + r) >> 1
        c += 1
        if (arr[mid] < item):
            l = mid + 1
        else:
            r = mid
    if (arr[l] != item):
        return (None, c)
    return (l, c)

def seq_search_compare(arr,item):
    print("Sequential Search: ", seq_searchSC(arr,item))
    print("For-loop: ", seq_searchFC(arr,item))
    print("While-loop: ", seq_searchWC(arr,item))
    
def test_ex4():
    for n in [1, 2, 3, 199, 400]:
        seq_search_compare(sorted_list, n)

def selection_sort(a):
    n = len(a)
    c = 0
    s = 0
    for i in range(n - 1):
        tmp = i
        for j in range(i + 1, n):
            c += 1
            if a[tmp] >= a[j]:
                tmp = j
        s += 1
        l = a[tmp]
        a[tmp] = a[i]
        a[i] = l
    return (c, s)

def test_ex5():
    for n in range(10):
        a = sorted_list
        print(selection_sort(a))

def bubble_sort(a):
    n = len(a)
    c = 0
    s = 0
    for i in range(n - 1):
        for j in range(n - 1):
            c += 1
            if a[j] > a[j + 1]:
                s += 1
                l = a[j]
                a[j] = a[j + 1]
                a[j + 1] = l
    return (c, s)

def bubble_sort_a(a):
    n = len(a)
    c = 0
    s = 0
    s = 0
    for i in range(n - 1):
        f = True
        for j in range(s, n - 1):
            c += 2
            if a[j] > a[j + 1]:
                f = False
                s += 1
                l = a[j]
                a[j] = a[j + 1]
                a[j + 1] = l
            if f:
                s = j
    return (c, s)

def bubble_sort_b(a):
    n = len(a)
    c = 0
    s = 0
    s = 0
    for i in range(n - 1):
        f = True
        for j in range(s, n - 1):
            c += 2
            if a[j] > a[j + 1]:
                f = False
                s += 1
                l = a[j]
                a[j] = a[j + 1]
                a[j + 1] = l
            if f:
                s = j
        c += 1
        if (f):
            break
    return (c, s)

def test_ex6():
    for n in range(10):
        a = sorted_list
        print(bubble_sort_b(a) == a)

def sort_compare(arr):
    print("selection_sort: ", selection_sort(arr))
    print("bubble_sort: ", bubble_sort(arr))
    print("bubble_sort_a: ", bubble_sort_a(arr))
    print("bubble_sort_b: ", bubble_sort_b(arr))

def test_ex7():
    for n in range(10):
        sort_compare(unsorted)    
    for n in range(10):
        sort_compare(unsorted1)
    for n in range(10):
        sort_compare(revsorted)
    
def binary_search_r(arr,item, l, r, c = 0):
    mid = (l + r) >> 1
    c += 3
    if (arr[mid] == item):
        return (mid, c)
    if r == 1:
        return -1
    if (arr[mid] < item):
        return binary_search_r(arr, item, mid, r, c)
    else:
        return binary_search_r(arr, item, l, mid, c)
    
def binary_search_w(arr,item):
    l = 0
    r = len(arr) - 1
    c = 0
    while (l < r):
        mid = (l + r) >> 1
        c += 1
        if (arr[mid] < item):
            l = mid + 1
        else:
            r = mid
    c += 1
    if (arr[l] != item):
        return (-1, c)
    return (l, c)

def seq_search_compare_b(arr,item):
    print("binary_search_w: ", binary_search_w(arr,item))
    print("binary_search_r: ", binary_search_r(arr,item, 0, len(arr)))
    
def test_ex10():
    for n in [1, 2, 3, 199, 400]:
        seq_search_compare_b(sorted_list, 3)

if __name__ == "__main__":
    #test_ex1()
    #test_ex2()
    #test_ex3()
    #test_ex4()
    #test_ex5()
    #test_ex6()
    #test_ex7()
    test_ex10()
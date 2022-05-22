#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 27 13:08:27 2020

@author: peixin.you
"""

def sum_list_for(arr):
    total = 0
    for i in range(0, len(arr)):
        total += arr[i]
    return total

def sum_list_while(arr):
    total = 0
    i = 0
    while (i < len(arr)):
        total += arr[i]
        i += 1
    return total

def q2():
    '''
    arr = [5, 2, 9, 3, 7, 4]
    this changes arr by the rule a[i] = a[(i - 1) mod n]
    '''
    pass


def reverse_list(arr):
    n = len(arr) - 1
    for i in range(0, (n + 1) // 2):
        tmp = arr[i]
        arr[i] = arr[n - i]
        arr[n - i] = tmp
    return arr

def reverse_list_part(arr,s,f):
    s -= 1; f -= 1
    for i in range(0, (f - s + 1) // 2):
        tmp = arr[s + i]
        arr[s + i] = arr[f - i]
        arr[f - i] = tmp
    return arr

def findMaxAndMin(arr):
    mn = 1000000007
    posMn = 0
    mx = -mn
    posMx = 0
    for i in range(0, len(arr)):
        if (mn > arr[i]):
            mn = arr[i]
            posMn = i
        if (mx < arr[i]):
            mx = arr[i]
            posMx = i
    return (mn, mx, posMn, posMx)

def isSorted(arr):
    flag = True
    for i in range(1, len(arr)):
        if arr[i] < arr[i - 1]:
            flag = False
    return flag

def count(arr, key):
    total = 0
    for i in range(0, len(arr)):
        if arr[i] <= key:
            total += 1
    return total

def mul(a, b):
    f = 1
    if (a < 0):
        a = -a
        f = -f
    if (b < 0):
        b = -b
        f = -1
    i = 1
    times = 0
    total = 0
    while (i <= a):
        if (i & a):
            total += b << times
        times += 1
        i <<= 1
    return total * f

def q10():
    '''
        + and * two one-digit numbers are basic operations
        and we will find that the time complexity of calculate
        n-digit number time a m-digit number is O(n*m)
    '''
    pass

def q11():
    '''
        since we can say that the bit operation is O(1) and for the
        while it only will run at most O(log n) times 
        So time complexity of calculate n-digit number time a m-digit 
        number is O(log n)
        So the first one is better.
    '''
    pass

def q12():
    '''
        we can see that 2242 * 1238 = (2200 + 42) * (1200 + 38) = 
        (22 * 12) * 10 * 10 * 10 * 10 + 
        (22 * 38) * 10 * 10 + 
        (42 * 12) * 10 * 10 +
        (42 * 38)
        
        So for abcd * efgh = (ab00 + cd) * (ef00 + gh) = 
        (ab * ef) * 10 * 10 * 10 * 10 +
        (ab * gh) * 10 * 10 +
        (cd * ef) * 10 * 10 + 
        (cd * gh)
    '''
    pass

def shift(arr, k):
    n = len(arr)
    arr = reverse_list_part(arr, 1, k)
    arr = reverse_list_part(arr, k + 1, n)
    arr = reverse_list_part(arr, 1, n)
    return arr

def test_q1():
    lists = [[],[2],[1,2,3],[1,7,3,2,9]]
    for i in lists:
        print("For-loop:",sum_list_for(i))
        print("While-loop:",sum_list_while(i))

def test_q3():
    print('1')
    lists = [[3], [1, 2, 3, 4], [2, 9, 7, 0, 3]]
    for i in lists:
        print(reverse_list(i))
    print('2')
    print(reverse_list_part([0], 1, 1))
    print(reverse_list_part([1, 2, 3, 4], 2, 4))
    print(reverse_list_part([1, 2, 3, 4], 1, 4))
    print(reverse_list_part([1, 2, 3, 4], 3, 3))

def test_q5():
    lists = [[-1, -1, -1, -1], [2], [1,2,3], [1,7,3,2,9], [1, 2, 3, 3, -1, -1, -1]]
    for i in lists:
        print(findMaxAndMin(i))

def test_q6():
    lists = [[-1, -1, -1, -1], [2], [1,2,3], [1,7,3,2,9], [1, 2, 3, 3, -1, -1, -1]]
    for i in lists:
        print(isSorted(i))
        
def test_q7():
    lists = [[1], [1,2,3], [1,7,3,2,9,4], [1, 2, 3, 7, -1, 5, 10]]
    print(count(lists[0], 1))
    print(count(lists[1], 2))
    print(count(lists[2], 3))
    print(count(lists[3], 10))

def test_q9():
    lists = [(1, 1), (0, 0), (-1, 1), (1, -1), (10, 1000), (5153, 435)]
    for i in lists:
        a, b = i
        print(mul(a, b))

def test_shift():
    print(shift([0,1,3,2,5], 1))
    print(shift([0,1,3,2,5], 2))
    print(shift([0,1,3,2,5], 3))
    print(shift([0,1,3,2,5], 4))

if __name__ == "__main__":
#    test_q1()
#    test_q3()
#    test_q5()
#    test_q6()
#    test_q7()
#    test_q9()
    test_shift()
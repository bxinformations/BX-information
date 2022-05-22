# -*- coding: utf-8 -*-
from chains import *

grading_mode = False

def test_powers(name, func):
    if func(1,0) is None:
        print("skipping {} (unimplemeneted)".format(name))
        assert not grading_mode
        return
    p2 = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048]
    p3 = [1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147]
    for i in range(len(p2)):
        res = func(2, i)
        if res != p2[i]:
            print("wrong value for 2^{}: should be {} but is {}".format(i, p2[i], res))
            assert not grading_mode
    for i in range(len(p3)):
        res = func(3, i)
        if res != p3[i]:
            print("wrong value for 3^{}: should be {} but is {}".format(i, p3[i], res))
            assert not grading_mode

def test1():
    test_powers("bin_power", bin_pow)

def test2():
    if cost_bin_pow(1) is None:
        print("skipping cost_bin_pow (unimplemeneted)")
        assert not grading_mode
        return
    res = [0, 0, 1, 2, 2, 3, 3, 4, 3, 4, 4]
    for i in range(len(res)):
        tmp = cost_bin_pow(i)
        if tmp != res[i]:
            print("wrong cost for power {}: should be {} but is {}".format(i, res[i], tmp))
            assert not grading_mode

def test3():
    if smallest_factor(3) is None:
        print("skipping smallest_factor (unimplemeneted)")
        assert not grading_mode
        return
    ll = [(2, -1), (3, -1), (4, 2), (5, -1), (9, 3), (31, -1), (1001, 7), (100000007, -1), (10007*10007, 10007)]
    for (number, factor) in ll:
        res = smallest_factor(number)
        if res != factor:
            print("wrong factor for {}: should be {} but is {}".format(number, factor, res))
            assert not grading_mode

def test4():
    test_powers("factor_pow", factor_pow)

def test5():
    if cost_factor_pow(1) is None:
        print("skipping cost_factor_pow (unimplemeneted)")
        assert not grading_mode
        return
    res = [0, 0, 1, 2, 2, 3, 3, 4, 3, 4, 4, 5, 4, 5, 5, 5, 4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 6, 6, 7, 6, 7, 5, 7, 6, 7, 6, 7, 7, 7, 6, 7, 7, 8, 7, 7, 8, 9, 6, 8]
    for i in range(len(res)):
        tmp = cost_factor_pow(i)
        if tmp != res[i]:
            print("wrong cost for power {}: should be {} but is {}".format(i, res[i], tmp))
            assert not grading_mode

def test6():
    if power_from_chain(2, [1, 2, 3, 6, 12, 15]) is None:
        print("skipping power_from_chain (unimplemeneted)")
        assert not grading_mode
        return
    res = power_from_chain(2, [1, 2, 3, 6, 12, 15])
    expected = 32768
    if res != expected:
        print("wrong value for power from chain: should be {} but is {}".format(expected, res))
        assert not grading_mode

test1()
test2()
test3()
test4()
test5()
test6()

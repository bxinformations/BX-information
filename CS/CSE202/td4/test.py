# -*- coding: utf-8 -*-

import exponential as expo
import owndiv as odiv
from gmpy2 import mpz

import __main__
grading_mode = __main__.__dict__.get('grading_mode', False)
print("grading mode is {}".format(grading_mode))

def test1():
    res = 6613313319248080001
    tmp = expo.numer_iter(20)
    if tmp is None:
        print("skipping numer_iter (unimplemented)")
        assert not grading_mode
        return
    if tmp != res:
        print("Your function numer_iter returned the wrong result ({} instead of 133) on 20".format(tmp, res))
        assert not grading_mode

def test2():
    matA = [[42, 58], [-36, 9]]
    matB = [[-32, 4], [90, -7]]
    res = [[3876, -238], [1962, -207]]
    tmp = expo.prod_mat(matA, matB)
    if tmp is None:
        print("skipping prod_mat (unimplemented)")
        assert not grading_mode
        return
    if tmp != res:
        print("Your function prod_mat returned the wrong result ({} instead of {}) on {} times {}".format(tmp, res, matA, matB))
        assert not grading_mode

def test2b():
    res = 6613313319248080001
    tmp = expo.numer_bin(20)
    if tmp is None:
        print("skipping numer_bin (unimplemented)")
        assert not grading_mode
        return
    if tmp != res:
        print("Your function numer_bin returned the wrong result ({} instead of {}) on 20".format(tmp, res))
        assert not grading_mode

def test3():
    tmp = expo.exp_digits(3)
    if tmp is None:
        print("skipping exp_digits (unimplemented)")
        assert not grading_mode
        return
    if tmp != 2718:
        print("Your function exp_digits returned the wrong result ({} instead of 2718) on 3".format(tmp))
        assert not grading_mode
    tmp = expo.exp_digits(345)
    res = mpz("2718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466391932003059921817413596629043572900334295260595630738132328627943490763233829880753195251019011573834187930702154089149934884167509244761460668082264800168477411853742345442437107539077744992069551702761838606261331384583000752044933826560") # the last digit is incorrect (makes no sense)
    # compare the first 300 digits
    if tmp // 10 != res // 10:
        correct = 0
        max_correct = 344
        while correct < max_correct:
            prec = mpz(10)**(max_correct - correct)
            if tmp // prec != res // prec:
                break
            correct += 1
        print("Your function exp_digits returned the wrong result: only the first {} out of {} digits are correct".format(correct, max_correct))
        assert not grading_mode

def dual_for_tests(x):    
    n=x.bit_length()
    return 2**(2*n-1)//x

def test4():
    for valin in [223456, 123456, 3422098899000]:
        tmp = odiv.dual_dac(valin)
        if tmp is None:
            print("skipping dual_dac (unimplemented)")
            assert not grading_mode
            return
        res = dual_for_tests(valin)
        if tmp != res:
            print("Your function dual_dac returned the wrong result ({} instead of {}) on {}".format(tmp, res, valin))
            assert not grading_mode

def test5():
    for (A,B) in [(10, 1), (9, 3), (1025, 33), (534543453964564094596465, 3422098899000)]:
        tmp = odiv.division(A, B)
        if tmp is None:
            print("skipping division (unimplemented)")
            assert not grading_mode
            return
        res = A // B
        if tmp != res:
            print("Your function division returned the wrong result ({} instead of {}) on {}/{}".format(tmp, res, A, B))
            assert not grading_mode

test1()
test2()
test2b()
test3()
test4()
test5()

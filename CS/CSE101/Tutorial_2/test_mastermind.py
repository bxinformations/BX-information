#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on:

@author: uli
"""

import mastermind

def test_create_code():
    """Test mastermind.create_code."""
    try:
        mastermind.COLORS
    except AttributeError:
        print('FAILED. It seems you have forgotten to define the global variable COLORS')
        return None
    if mastermind.COLORS != ['RED', 'GREEN', 'BLUE', 'PURPLE', 'BROWN', 'YELLOW']:
        print("FAILED. Please set your global variable COLORS to ['RED', 'GREEN', 'BLUE', 'PURPLE', 'BROWN', 'YELLOW']")
        return None
    try:
        out = mastermind.create_code()
    except Exception as e:
        print('FAILED with exception', e, 'when running create.code()')
        return None
    if type(out) != list:
        print('FAILED. create_code() did not return a list')
        return None
    for p in out:
        if p not in mastermind.COLORS:
            print('FAILED. create_code() returned', p, 'which is not in COLORS')
            return None
    d = {p:0 for p in mastermind.COLORS}
    NUMTESTS = 1000
    for _ in range(NUMTESTS):
        out = mastermind.create_code()
        for p in out:
            d[p] += 1
    maxdiff = max([d[p] for p in d]) - min([d[p] for p in d])
    if maxdiff > NUMTESTS // 10:
        print('Running create_code()', NUMTESTS, 'times produced the following distribution of pins:\n',
              d, '\nIt is very unlikely that your create_code() function is uniformly random.')
        return None
    print("All tests passed.")
    print('(Congratulations!)')
    
def test_black_pins():
    """Test mastermind.black_pins."""
    tests = [
            ((['RED', 'BLUE', 'YELLOW', 'BROWN'], ['RED', 'RED', 'GREEN', 'YELLOW']), (1, ['BLUE', 'YELLOW', 'BROWN'], ['RED', 'GREEN', 'YELLOW'])),
            ((['RED', 'BLUE', 'BROWN', 'YELLOW'], ['RED', 'RED', 'GREEN', 'YELLOW']), (2, ['BLUE', 'BROWN'], ['RED', 'GREEN'])),
            ((['RED', 'BLUE', 'YELLOW', 'YELLOW'], ['RED', 'RED', 'GREEN', 'YELLOW']), (2, ['BLUE', 'YELLOW'], ['RED', 'GREEN'])),
            ((['RED', 'RED', 'YELLOW', 'YELLOW'], ['RED', 'RED', 'GREEN', 'YELLOW']), (3, ['YELLOW'], ['GREEN'])),
            ((['RED', 'RED', 'GREEN', 'YELLOW'], ['RED', 'RED', 'GREEN', 'YELLOW']), (4, [], [])),
            ]
    if not generic_tester(mastermind.black_pins, tests):
        return False
    print('Testing whether your black_pins() modifies `code`...')
    def fake_black_pins(guess, code):
        mastermind.black_pins(guess, code)
        return code
    tests = [
            ((['RED', 'BLUE', 'YELLOW', 'BROWN'], ['RED', 'RED', 'GREEN', 'YELLOW']), ['RED', 'RED', 'GREEN', 'YELLOW']),
            ((['RED', 'BLUE', 'BROWN', 'YELLOW'], ['RED', 'RED', 'GREEN', 'YELLOW']), ['RED', 'RED', 'GREEN', 'YELLOW']),
            ((['RED', 'BLUE', 'YELLOW', 'YELLOW'], ['RED', 'RED', 'GREEN', 'YELLOW']), ['RED', 'RED', 'GREEN', 'YELLOW']),
            ((['RED', 'RED', 'YELLOW', 'YELLOW'], ['RED', 'RED', 'GREEN', 'YELLOW']), ['RED', 'RED', 'GREEN', 'YELLOW']),
            ((['RED', 'RED', 'GREEN', 'YELLOW'], ['RED', 'RED', 'GREEN', 'YELLOW']), ['RED', 'RED', 'GREEN', 'YELLOW']),
            ]
    generic_tester(fake_black_pins, tests)
    
def test_score_guess():
    tests = [
            ((['RED', 'GREEN', 'YELLOW', 'BLUE'], ['RED', 'YELLOW', 'BROWN', 'RED'],), (1, 1)),
            ((['RED', 'GREEN', 'YELLOW', 'YELLOW'], ['RED', 'YELLOW', 'BROWN', 'RED']), (1, 1)),
            ((['RED', 'RED', 'RED', 'YELLOW'], ['RED', 'YELLOW', 'BROWN', 'RED']), (1, 2)),
            ((['RED', 'RED', 'RED', 'YELLOW'], ['RED', 'YELLOW', 'RED', 'RED']), (2, 2)),
            ((['RED', 'RED', 'RED', 'YELLOW'], ['RED', 'YELLOW', 'RED', 'BLUE']), (2, 1)),
            ]
    generic_tester(mastermind.score_guess, tests)

def test_input_guess():
    """Test mastermind.input_guess."""
    #decorate mastermind.input_guess to override input & print
    def fake_input_guess(l):
        itr = iter(l)
        def fake_input(*s):
            return next(itr)
        def fake_print(*s):
            pass
        mastermind.input = fake_input
        mastermind.print = fake_print
        return mastermind.input_guess()
    tests = [
            ((['RED', 'BLUE', 'YELLOW', 'BROWN'],), ['RED', 'BLUE', 'YELLOW', 'BROWN']),
            ((['RED', 'BLUE', 'MAUVE', 'YELLOW', 'BROWN'],), ['RED', 'BLUE', 'YELLOW', 'BROWN']),
            ((['red', 'RED', 'Red', 'RED', 'REd', 'RED', 'reD', 'RED'],), ['RED', 'RED', 'RED', 'RED']),
            ]
    generic_tester(fake_input_guess, tests)

def test_one_round():
    """Test mastermind.one_round."""
    def fake_one_round(l, c):
        itr = iter(l)
        def fake_input(*s):
            return next(itr)
        def fake_print(*s):
            pass
        mastermind.input = fake_input
        mastermind.print = fake_print
        return mastermind.one_round(c)
    tests = [
            ((['RED', 'GREEN', 'YELLOW', 'BLUE'], ['RED', 'YELLOW', 'BROWN', 'RED'],), False),
            ((['RED', 'RED', 'RED', 'YELLOW'], ['RED', 'YELLOW', 'RED', 'RED']), False),
            ((['RED', 'RED', 'RED', 'YELLOW'], ['RED', 'RED', 'RED', 'YELLOW']), True),
            ]
    generic_tester(fake_one_round, tests)

def generic_tester(f,tests):
    """Generic testing procedure.
    Iterate over a dictionary of tests 
    (keys = argument tuples, values = expected outputs),
    run function f on them, and complain where appropriate.
    """
    print('Testing {}...'.format(f))
    counter = 0
    passed = 0
    failed = 0
    for (test_args,test_out) in tests:
        print('  Test #{} '.format(counter),end='')
        counter += 1
        try:
            real_out = f(*test_args)
            if real_out != test_out:
                failed += 1
                print('FAILED (arguments = {}: expected {}, got {})'.format(
                        test_args,test_out,real_out))
            else:
                passed += 1
                print('passed')
        except Exception as e:
            failed += 1
            print(('FAILED with exception {}.'
                   + ' Try debugging with arguments = {}'
                   + ' (expected output = {})').format(e,test_args,test_out))
    print('{} tests run, {} passed, {} failed'.format(
            counter,
            passed,
            failed))    
    if failed == 0:
        print('(Congratulations!)')
        return True
    else:
        return False
    

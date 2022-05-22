# -*- coding: utf-8 -*-
import collections
import io
from contextlib import redirect_stdout
import recursion

def generic_tester(f,tests):
    """Generic testing procedure.
    Iterate over a dictionary of tests
    (keys = argument tuples, values = expected outputs),
    run function f on them, and complain where appropriate.
    """
    print('Testing {}...'.format(f))
    counter = 0
    failed = 0
    for (arguments,test_out) in tests.items():
        counter += 1
        try:
            real_out = f(*arguments)
            if real_out != test_out:
                failed += 1
                print('  Test #{} FAILED:\nArguments = {}\nExpected {}\nGot      {})'.format(
                        counter, arguments, test_out, real_out))
        except Exception as e:
            failed += 1
            print(('  Test #{} FAILED with exception {}.'
                   + ' Try debugging with arguments = {}'
                   + ' (expected output = {})').format(counter,e,arguments,test_out))
    print('{} tests run, {} passed, {} failed'.format(
            counter,
            counter - failed,
            failed))
    if failed == 0:
        print('(Congratulations!)')

def file_line_count(filename):
    """Count the number of lines in a file."""
    with open(filename) as myfile:
        return sum(1 for line in myfile)

def generic_output_file_without_order_tester(f, arguments, outputfile, ref_outputfile):
    """Run the function f on the arguments and then compare the specified
    output file to the reference file"""
    print('Testing {}...'.format(f), end=' ')
    passed = True
    try:
        f(*arguments)
        ref_line_count = file_line_count(ref_outputfile)
        f_line_count = file_line_count(outputfile)
        if (ref_line_count != f_line_count):
            print('\nFAILED (wrong number of lines: {} {}, but {} {}'.format(
                    outputfile, f_line_count, ref_outputfile, ref_line_count))
            passed = False
        else:
            with open(outputfile) as ofile, open(ref_outputfile) as rfile:
                reference = rfile.readlines()
                counter = 1
                for oline in ofile:
                    if oline not in reference:
                        print(('\nFAILED (in output line {},{}:\n'
                            + 'did not find {} in reference file\n').format(
                                counter, outputfile, oline.strip()))
                        passed = False
                        break
                    counter += 1
        if passed:
            print('passed!')
            print('Congratulations!')
    except Exception as e:
        print(('\nFAILED with exception {}.'
                + ' Try debugging with arguments = {}').format(
                        e,arguments))

def generic_value_tester(functions, expected):
    """Generic testing procedure.
    Receive a functions creating elements and compare them for equality.
    """
    print('Testing...')
    counter = 0
    failed = 0
    for (function, expected_result) in zip(functions, expected):
        counter += 1
        try:
            real_out = function()
            if real_out != expected_result:
                failed += 1
                print('  Test #{} FAILED:\nRan "{}"\nExpected "{}"\nGot      "{}"'.format(
                        counter, function.__name__, expected_result, real_out))
        except Exception as e:
            failed += 1
            print('  Test #{} FAILED with exception "{}" when running "{}".'.format(
                    counter, e, function.__name__))
    print('{} tests run, {} passed, {} failed'.format(
            counter,
            counter - failed,
            failed))
    if failed == 0:
        print('(Congratulations!)')

def unordered_print_tester(f, args):
    """Return a counter object counting the lines printed
    to standard output by the function f with arguments args."""
    stream = io.StringIO()
    with redirect_stdout(stream):
        f(*args)
    return collections.Counter(stream.getvalue().split('\n')[:-1])

class countcalls(object):
    "Decorator that keeps track of the number of times a function is called."

    __instances = {}
    def __init__(self, f):
       self.__f = f
       self.__numcalls = 0
       countcalls.__instances[f] = self

    def __call__(self, *args, **kwargs):
       self.__numcalls += 1
       return self.__f(*args, **kwargs)

    def count(self):
       "Return the number of times the function f was called."
       return countcalls.__instances[self.__f].__numcalls

    def reset(self):
        "Reset the counter."
        self.__numcalls = 0

    @staticmethod
    def counts():
       "Return a dict of {function: # of calls} for all registered functions."
       return dict([(f.__name__, countcalls.__instances[f].__numcalls) for f in countcalls.__instances])

def gcd_helper(a, b, expected_recursion_count):
    """Return the gcd and the number of calls to gcd."""
    tmp = recursion.gcd
    recursion.gcd = countcalls(recursion.gcd)
    res = recursion.gcd(a, b)
    count = recursion.gcd.count()
    recursion.gcd = tmp
    #print('Debug: number of recursion calls: expected {}, got {}'.format(expected_recursion_count, count))
    if abs(expected_recursion_count - count) > 2:
        print('Wrong number of recursion calls: expected {}, got {}'.format(expected_recursion_count, count))
        return False
    return res

def gcd_tester(f,tests):
    """Generic testing procedure.
    Iterate over a dictionary of tests
    (keys = argument tuples, values = expected outputs),
    run function f on them, and complain where appropriate.
    """
    print('Testing {}...'.format(f))
    counter = 0
    failed = 0
    for (arguments,test_out) in tests.items():
        real_arguments = arguments
        arguments = arguments[:-1]        
        counter += 1
        try:
            real_out = f(*real_arguments)
            if real_out != test_out:
                failed += 1
                print('  Test #{} FAILED:\nArguments = {}\nExpected {}\nGot      {})'.format(
                        counter, arguments, test_out, real_out))
        except Exception as e:
            failed += 1
            print(('  Test #{} FAILED with exception {}.'
                   + ' Try debugging with arguments = {}'
                   + ' (expected output = {})').format(counter,e,arguments,test_out))
    print('{} tests run, {} passed, {} failed'.format(
            counter,
            counter - failed,
            failed))
    if failed == 0:
        print('(Congratulations!)')


def test_gcd():
    tests = {(2,5,4): 1, (5,2,3): 1, (12, 4,2): 4, (1000001, 53785,11):1,
             (0,10,1):10, (10,0,1):10}
    gcd_tester(gcd_helper, tests)

def palindrome_helper(word):
    """Return the number of calls to palindrome."""
    tmp = recursion.is_palindrome
    recursion.is_palindrome = countcalls(recursion.is_palindrome)
    recursion.is_palindrome(word)
    count = recursion.is_palindrome.count()
    recursion.is_palindrome = tmp
    return count

def test_palindrome():
    print('Tests Phase 1: Correctness')
    test = {('aa',): True, ('a',): True, ('ab',): False, ('aba',): True, ('aab',): False, ('aaaaaa',): True}
    generic_tester(recursion.is_palindrome, test)

    print('Tests Phase 2: Testing recursion depth')
    test = {('aaaaaa',): 4, ('aaaaab',): 1, ('abcba',): 3}
    generic_tester(palindrome_helper, test)

def test_binary_search():
    test = {((1,2,3,4,5,6,7), 0, 6, 3): 2, ((1,2,3,5,6,7), 0, 6, 4): -1,
            ((1,2,3,4,5,6,7), 0, 6, 1): 0, ((1,2,3,4,5,6,7), 0, 7, 7): 6,
            ((1,2,3,4,5,6,7), 0, 2, 3): -1, ((1,2,3,4,5,6,7), 3, 6, 3): -1}
    generic_tester(recursion.binary_search, test)

def test_read_integer():
    test = {('123', 0): (123,3), ('q123', 0): None, ('q123', 1): (123, 4), ('123q', 0): (123, 3)}
    generic_tester(recursion.read_positive_integer, test)

def test_evaluate():
    test = { ('10',0): (10,2), ('(1+1)',0): (2,5), ('(1*1)',0): (1,5), ('(1-1)',0): (0,5), ('((1-1)+(2*1))',0): (2,13),
            ('10+2',0): (10,2), ('(1+0))',0): (1,5)}
    generic_tester(recursion.evaluate, test)
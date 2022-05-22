# -*- coding: utf-8 -*-

import primary
import io
import os
#import collections
from contextlib import redirect_stdout

def generic_equality(x, y):
    return x == y

def approx_eq_float(f1, f2):
    if type(f1) != float and type(f1) != int:
        return False
    if type(f2) != float and type(f2) != int:
        return False
    return abs(f1 - f2) < 0.01

def approx_eq_dict_of_floats(d1, d2):
    if type(d1) != dict or type(d2) != dict:
        return False
    if set(d1.keys()) != set(d2.keys()):
        return False
    for k in d1:
        if not approx_eq_float(d1[k], d2[k]):
            return False
    return True

def approx_eq_float_x_dict_of_floats(t1, t2):
    """Check equality of (thing, {thing: float}).
    """
    if type(t1) != tuple or len(t1) != 2:
        return False
    if type(t2) != tuple or len(t2) != 2:
        return False
    if not approx_eq_float(t1[0], t2[0]):
        return False
    if not approx_eq_dict_of_floats(t1[1], t2[1]):
        return False
    return True

def approx_eq_dict_of_float_x_dict_of_floats(d1, d2):
    """Check equality of {thing: (float, {thing: float})}.
    """
    if type(d1) != dict or type(d2) != dict:
        return False
    if set(d1.keys()) != set(d2.keys()):
        return False
    for k in d1:
        if not approx_eq_float_x_dict_of_floats(d1[k], d2[k]):
            return False
    return True

def generic_tester(f, tests, eq):
    """Generic testing procedure.
    Iterate over a dictionary of tests 
    (keys = argument tuples, values = expected outputs),
    run function f on them, and complain where eq(expected, got) != True.
    """
    print('Testing {}...'.format(f))
    counter = 0
    passed = 0
    failed = 0
    for (arguments,test_out) in tests:
        print('  Test #{} '.format(counter),end='')
        counter += 1
        try:
            real_out = f(*arguments)
            if not eq(real_out, test_out):
                failed += 1
                print('FAILED (arguments = {}: expected {}, got {})'.format(
                        arguments,test_out,real_out))
            else:
                passed += 1
                print('passed')
        except Exception as e:
            failed += 1
            print(('FAILED with exception {}.'
                   + ' Try debugging with arguments = {}'
                   + ' (expected output = {})').format(e,arguments,test_out))
        if failed > 0:
            break
    print('{} tests run out of {}: {} passed, {} failed'.format(
            counter,
            len(tests),
            passed,
            failed))
    if failed == 0:
        print('(Congratulations!)')

def exist_and_are_readable(filenames):
    """Return True iff every file named in the list exists and is
    readable.  Prints the names of all unreadable files in the list."""
    ok = True
    for filename in filenames:
        if not os.access(filename,os.R_OK):
            print('ERROR: could not read required file ' + filename)
            ok = False
    return ok

def print_tester(f, args):
    """Return a counter object counting the lines printed 
    to standard output by the function f with arguments args."""
    stream = io.StringIO()
    with redirect_stdout(stream):
        f(*args)
    return stream.getvalue().split('\n')[:-1]

def test_print_approvals():
    """Test primary.print_approvals.
    """
    raw_tests = [
                 (([('A', 1), ('B',2.0), ('C',3), ('D',4.5)],),
                  ['A 1%', 'B 2.0%', 'C 3%', 'D 4.5%']),
                 (([('Bernie Sanders', 33.0), ('Elizabeth Warren', 35.1)],),
                  ['Bernie Sanders 33.0%', 'Elizabeth Warren 35.1%']), 
                ]
    tests = [((primary.print_approvals, args), res) 
               for (args, res) in raw_tests ]
    generic_tester(print_tester, tests, generic_equality)

def test_read_candidate_ids():
    """Test primary.read_candidate_ids.
    """
    short = ['Elizabeth Warren',
             'Kamala Harris',
             'Cory Booker',
             'Bernie Sanders']
    tests = [(("short_primary.txt",), short),]
    if not exist_and_are_readable(test[0][0] for test in tests):
        print('ERROR: Tests not run')
        return None
    generic_tester(primary.read_candidate_ids, tests, generic_equality)
    
def test_filter_candidates():
    """Test primary.filter_candidates.
    """
    cands = ['Elizabeth Warren',
                 'Kamala Harris',
                 'Cory Booker',
                 'Bernie Sanders',
                 ]
    short_out = ['Elizabeth Warren',
                 'Kamala Harris',
                 'Cory Booker',
                 'Bernie Sanders',]
    full_out =  ['Elizabeth Warren',
                 'Kamala Harris',
                 'Cory Booker',
                 'Bernie Sanders',]
    tests = [((cands, 'short_opinion.txt',), short_out),
             ((cands, 'opinion.txt',), full_out),]
    if not exist_and_are_readable(test[0][1] for test in tests):
        print('ERROR: Tests not run')
        return None
    generic_tester(primary.filter_candidates, tests, generic_equality)

def test_read_polls():
    cands = ['Elizabeth Warren',
                 'Kamala Harris',
                 'Cory Booker',
                 'Bernie Sanders',
                 'Joe Biden',
                 ]

    out_short = {'CNN': {'Bernie Sanders': 33.0,
                         'Elizabeth Warren': 25.0,
                         'Kamala Harris': 3.2},
                 'FOX': {'Bernie Sanders': 1.4, 'Elizabeth Warren': 12.3},
                 'MSNBC': {'Bernie Sanders': 24.1,
                           'Cory Booker': 4.3,
                           'Elizabeth Warren': 25.7}}
    out_full = {'CNN': {'Bernie Sanders': 33.0,
                        'Elizabeth Warren': 25.0,
                        'Joe Biden': 29.0,
                        'Kamala Harris': 3.2},
                'FOX': {'Bernie Sanders': 1.4, 'Elizabeth Warren': 12.3},
                'MSNBC': {'Bernie Sanders': 24.1,
                          'Cory Booker': 4.3,
                          'Elizabeth Warren': 25.7,
                          'Joe Biden': 28.2},
                'NYT': {'Bernie Sanders': 25.0,
                        'Cory Booker': 2.1,
                        'Elizabeth Warren': 26.8,
                        'Joe Biden': 30.0,
                        'Kamala Harris': 2.4}}
    tests = [((cands, 'short_opinion.txt',), out_short),
             ((cands, 'opinion.txt',), out_full),]
    if not exist_and_are_readable(test[0][1] for test in tests):
        print('ERROR: Tests not run')
        return None
    generic_tester(primary.read_polls, tests, generic_equality)
    
def test_opinions_by_candidate():
    """Test primary.opinions_by_candidate.
    """
    in_short = {'CNN': {'Bernie Sanders': 33.0,
                        'Elizabeth Warren': 25.0,
                        'Kamala Harris': 3.2},
                'FOX': {'Bernie Sanders': 1.4, 'Elizabeth Warren': 12.3},
                'MSNBC': {'Bernie Sanders': 24.1,
                          'Cory Booker': 4.3,
                          'Elizabeth Warren': 25.7}}
    out_short = {'Bernie Sanders': {'CNN': 33.0, 'FOX': 1.4, 'MSNBC': 24.1},
                 'Cory Booker': {'MSNBC': 4.3},
                 'Elizabeth Warren': {'CNN': 25.0, 'FOX': 12.3, 'MSNBC': 25.7},
                 'Kamala Harris': {'CNN': 3.2}}
    in_full = {'CNN': {'Bernie Sanders': 33.0,
                       'Elizabeth Warren': 25.0,
                       'Joe Biden': 29.0,
                       'Kamala Harris': 3.2},
               'FOX': {'Bernie Sanders': 1.4, 'Elizabeth Warren': 12.3},
               'MSNBC': {'Bernie Sanders': 24.1,
                         'Cory Booker': 4.3,
                         'Elizabeth Warren': 25.7,
                         'Joe Biden': 28.2},
               'NYT': {'Bernie Sanders': 25.0,
                       'Cory Booker': 2.1,
                       'Elizabeth Warren': 26.8,
                       'Joe Biden': 30.0,
                       'Kamala Harris': 2.4}}
    out_full = {'Bernie Sanders': {'CNN': 33.0, 'FOX': 1.4, 'MSNBC': 24.1, 'NYT': 25.0},
                'Cory Booker': {'MSNBC': 4.3, 'NYT': 2.1},
                'Elizabeth Warren': {'CNN': 25.0, 'FOX': 12.3, 'MSNBC': 25.7, 'NYT': 26.8},
                'Joe Biden': {'CNN': 29.0, 'MSNBC': 28.2, 'NYT': 30.0},
                'Kamala Harris': {'CNN': 3.2, 'NYT': 2.4}}
    tests = [((in_short,), out_short),
             ((in_full,), out_full),]
    generic_tester(primary.opinions_by_candidate, tests, generic_equality)

def test_average_approvals():
    """Test primary.average_approvals.
    """
    in_short = {'Bernie Sanders': {'CNN': 33.0, 'FOX': 1.4, 'MSNBC': 24.1},
             'Cory Booker': {'MSNBC': 4.3},
             'Elizabeth Warren': {'CNN': 25.0, 'FOX': 12.3, 'MSNBC': 25.7},
             'Kamala Harris': {'CNN': 3.2}}
    out_short = {'Bernie Sanders': 19.5,
                 'Cory Booker': 4.3,
                 'Elizabeth Warren': 21.0,
                 'Kamala Harris': 3.2}
    in_full = {'Bernie Sanders': {'CNN': 33.0, 'FOX': 1.4, 'MSNBC': 24.1, 'NYT': 25.0},
               'Cory Booker': {'MSNBC': 4.3, 'NYT': 2.1},
               'Elizabeth Warren': {'CNN': 25.0, 'FOX': 12.3, 'MSNBC': 25.7, 'NYT': 26.8},
               'Joe Biden': {'CNN': 29.0, 'MSNBC': 28.2, 'NYT': 30.0},
               'Kamala Harris': {'CNN': 3.2, 'NYT': 2.4}}
    out_full = {'Bernie Sanders': 20.875,
                'Cory Booker': 3.2,
                'Elizabeth Warren': 22.45,
                'Joe Biden': 29.066666666666666,
                'Kamala Harris': 2.8}
    tests = [((in_short,), out_short),
             ((in_full,), out_full),]
    generic_tester(primary.average_approvals, tests, approx_eq_dict_of_floats)
    
def test_top_outsider():
    """Test primary.top_outsider().
    """
    short = {'CNN': {'Bernie Sanders': 33.0,
                     'Elizabeth Warren': 25.0,
                     'Kamala Harris': 3.2},
             'FOX': {'Bernie Sanders': 1.4, 'Elizabeth Warren': 12.3},
             'MSNBC': {'Bernie Sanders': 24.1,
                       'Cory Booker': 4.3,
                       'Elizabeth Warren': 25.7}}
    full = {'CNN': {'Bernie Sanders': 33.0,
                    'Elizabeth Warren': 25.0,
                    'Joe Biden': 29.0,
                    'Kamala Harris': 3.2},
            'FOX': {'Bernie Sanders': 1.4, 'Elizabeth Warren': 12.3},
            'MSNBC': {'Bernie Sanders': 24.1,
                      'Cory Booker': 4.3,
                      'Elizabeth Warren': 25.7,
                      'Joe Biden': 28.2},
            'NYT': {'Bernie Sanders': 25.0,
                    'Cory Booker': 2.1,
                    'Elizabeth Warren': 26.8,
                    'Joe Biden': 30.0,
                    'Kamala Harris': 2.4}}
    tests = [((short, 'CNN'), 'Cory Booker'),
             ((short, 'FOX'), 'Cory Booker'),
             ((short, 'MSNBC'), 'Kamala Harris'),
             ((full, 'NYT'), None)]
    generic_tester(primary.top_outsider, tests, generic_equality)
    
def test_distance_from_average():
    """Test primary.distance_from_average().
    """
    short = {'CNN': {'Bernie Sanders': 33.0,
                     'Elizabeth Warren': 25.0,
                     'Kamala Harris': 3.2},
             'FOX': {'Bernie Sanders': 1.4, 'Elizabeth Warren': 12.3},
             'MSNBC': {'Bernie Sanders': 24.1,
                       'Cory Booker': 4.3,
                       'Elizabeth Warren': 25.7}}
    short_CNN_out = {'Bernie Sanders': 20.25, 'Elizabeth Warren': 6.0}
    short_FOX_out = {'Bernie Sanders': -27.15, 'Elizabeth Warren': -13.05}
    short_MSNBC_out = {'Bernie Sanders': 6.9, 'Elizabeth Warren': 7.05}
    tests = [((short, 'FOX'), short_FOX_out),
             ((short, 'CNN'), short_CNN_out),
             ((short, 'MSNBC'), short_MSNBC_out),]
    generic_tester(primary.distance_from_average,
                   tests,
                   approx_eq_dict_of_floats)
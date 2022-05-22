#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 28 00:29:56 2017

@author: smith
"""

import dates

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
    
    
def test_name_of_month():
    """Tests dates.name_of_month."""
    tests = [
            ((1,), 'January'),
            ((2,), 'February'),
            ((3,), 'March'),
            ((4,), 'April'),
            ((5,), 'May'),
            ((6,), 'June'),
            ((7,), 'July'),
            ((8,), 'August'),
            ((9,), 'September'),
            ((10,), 'October'),
            ((11,), 'November'),
            ((12,), 'December'),
            ((-1,), None),
            ((0,), None),
            ((17,), None),
            ]
    generic_tester(dates.name_of_month,tests)

def test_str_with_suffix():
    """ Test dates.str_with_suffix """
    tests = [
            ((0,), '0th'),
            ((1,), '1st'),
            ((2,), '2nd'),
            ((3,), '3rd'),
            ((4,), '4th'),
            ((111,), '111th'),
            ((121,), '121st'),
            ((212,), '212th'),
            ((222,), '222nd'),
            ((313,), '313th'),
            ((333,), '333rd'),
            ((414,), '414th'),
            ((444,), '444th'),
            ]
    generic_tester(dates.str_with_suffix,tests)

def test_is_leap_year():
    """Test dates.is_leap_year."""
    tests = [
            ((0,), True),
            ((1300,), False),
            ((1301,), False),
            ((1304,), True),
            ((1600,), True),
            ((17001,), False),
            ((17002,), False),
            ((17003,), False),
            ((17004,), True),
            ]
    generic_tester(dates.is_leap_year, tests)

def test_number_of_days():
    """Test dates.number_of_days."""
    tests = [
            ((1,1),31),
            ((2,1300),28),
            ((2,1304),29),
            ((2,1600),29),
            ((3,1600),31),
            ((9,1600),30),
            ((12,1600),31),
            ]
    generic_tester(dates.number_of_days, tests)

def test_date_string():
    """ Test dates.date_string """
    tests = [
            ((28,9,2017), 'The 28th of September, 2017'),
            ((31,12,1999), 'The 31st of December, 1999'),
            ((2,3,786), 'The 2nd of March, 786'),
            ((29,2,2000), 'The 29th of February, 2000'),
            ((29,2,1800), 'Nonexistent date'),
            ((33,1,1979), 'Nonexistent date'),
            ((12,0,105), 'Nonexistent date'),
            ((11,11,1918), 'The 11th of November, 1918'),
            ((1,1,1901), 'The 1st of January, 1901'),
            ((32,1,1901), 'Nonexistent date'),
            ]
    generic_tester(dates.date_string, tests)

def test_time_string():
    """ Test dates.time_string """
    tests = [
            ((0,), '0 seconds'),
            ((1,), '1 second'),
            ((10,), '10 seconds'),
            ((100,), '1 minute, 40 seconds'),
            ((1000,), '16 minutes, 40 seconds'),
            ((10000,), '2 hours, 46 minutes, 40 seconds'),
            ((1000000,), '11 days, 13 hours, 46 minutes, 40 seconds'),
            ((100000000,), '1157 days, 9 hours, 46 minutes, 40 seconds'),
            ((3601,), '1 hour, 1 second'),
            ((7201,), '2 hours, 1 second'),
            ((86461,), '1 day, 1 minute, 1 second'),
            ((90007,), '1 day, 1 hour, 7 seconds'),
            ]
    generic_tester(dates.time_string,tests)

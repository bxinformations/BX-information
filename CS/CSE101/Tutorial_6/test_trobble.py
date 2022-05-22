# -*- coding: utf-8 -*-
import trobble

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
    for (arguments,test_out) in tests:
        print('  Test #{} '.format(counter),end='')
        counter += 1
        try:
            real_out = f(*arguments)
            if real_out != test_out:
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
    print('{} tests run, {} passed, {} failed'.format(
            counter,
            passed,
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
    passed = 0
    failed = 0
    for (function, expected_result) in zip(functions, expected):
        print('  Test #{} '.format(counter),end='')
        counter += 1
        try:
            real_out = function()
            if real_out != expected_result:
                failed += 1
                print('FAILED (expected {}, got {})'.format(
                        expected_result,real_out))
            else:
                passed += 1
                print('passed')
        except Exception as e:
            failed += 1
            print('FAILED with exception {}.'.format(e))
    print('{} tests run, {} passed, {} failed'.format(
            counter,
            passed,
            failed))
    if failed == 0:
        print('(Congratulations!)')

def init1():
    t = trobble.Trobble('Dave', 'male')
    return t.__dict__

def init2():
    t = trobble.Trobble('Dave', 'male')
    t.age = 10
    return t.__dict__

def init3():
    t = trobble.Trobble('Louise', 'female')
    return t.__dict__

def test_init():
    functions = [init1, init2, init3]
    values = [{'age': 0, 'health': 10, 'sex': 'male', 'name': 'Dave', 'hunger':0},
              {'age': 10, 'health': 10, 'sex': 'male', 'name': 'Dave', 'hunger':0},
              {'age': 0, 'health': 10, 'sex': 'female', 'name': 'Louise', 'hunger':0}
                ]
    generic_value_tester(functions, values)

def str1():
    t = trobble.Trobble('Dave', 'male')
    return str(t)

def str2():
    t = trobble.Trobble('Dave', 'male')
    t.hunger = 10
    t.age = 12
    t.health = 0
    return str(t)

def str3():
    t = trobble.Trobble('Louise', 'female')
    return str(t)

def test_str():
    functions = [str1, str2, str3]
    values = ['Dave: male, health 10, hunger 0, age 0',
              'Dave: male, health 0, hunger 10, age 12',
              'Louise: female, health 10, hunger 0, age 0'
              ]
    generic_value_tester(functions, values)

def next1():
    t = trobble.Trobble('Dave', 'male')
    t.next_turn()
    return str(t)

def next2():
    t = trobble.Trobble('Dave', 'male')
    for i in range(5):
        t.next_turn()
    return str(t)

def next3():
    t = trobble.Trobble('Dave', 'male')
    for i in range(8):
        t.next_turn()
    return str(t)

def next4():
    t = trobble.Trobble('Dave', 'male')
    t.health = 0
    t.next_turn()
    return str(t)

def next5():
    t = trobble.Trobble('Dave', 'male')
    t.health = 1
    t.hunger = 100
    t.age = 1
    t.next_turn()
    return str(t)

def test_next_turn():
    functions = [next1, next2, next3, next4, next5]
    values = ['Dave: male, health 10, hunger 1, age 1',
              'Dave: male, health 10, hunger 15, age 5',
              'Dave: male, health 7, hunger 36, age 8',
              'Dave: male, health 0, hunger 0, age 0',
              'Dave: male, health 0, hunger 102, age 2']
    generic_value_tester(functions, values)

def feed1():
    t = trobble.Trobble('Dave', 'male')
    t.feed()
    return str(t)

def feed2():
    t = trobble.Trobble('Dave', 'male')
    t.hunger = 30
    t.feed()
    return str(t)

def feed3():
    t = trobble.Trobble('Dave', 'male')
    t.hunger = 10
    t.feed()
    return str(t)

def cure1():
    t = trobble.Trobble('Dave', 'male')
    t.cure()
    return str(t)

def cure2():
    t = trobble.Trobble('Dave', 'male')
    t.health = 2
    t.cure()
    return str(t)

def cure3():
    t = trobble.Trobble('Dave', 'male')
    t.health = 8
    t.cure()
    return str(t)

def alive1():
    t = trobble.Trobble('Dave', 'male')
    return t.is_alive()

def alive2():
    t = trobble.Trobble('Dave', 'male')
    t.health = 0
    return t.is_alive()

def test_care():
    functions = [feed1, feed2, feed3, cure1, cure2, cure3, alive1, alive2]
    values = ['Dave: male, health 10, hunger 0, age 0',
              'Dave: male, health 10, hunger 5, age 0',
              'Dave: male, health 10, hunger 0, age 0',
              'Dave: male, health 10, hunger 0, age 0',
              'Dave: male, health 7, hunger 0, age 0',
              'Dave: male, health 10, hunger 0, age 0',
              True, False]
    generic_value_tester(functions, values)

def test_mate():
    trobble.Trobble.__eq__ = lambda self, other: self.__dict__ == other.__dict__
    t1 = trobble.Trobble('1', 'male')
    t2 = trobble.Trobble('2', 'female')
    t3 = trobble.Trobble('3', 'male')
    t4 = trobble.Trobble('4', 'male')
    t3.age = 3
    t4.age = 4
    t5 = trobble.Trobble('5', 'female')
    t6 = trobble.Trobble('6', 'female')
    t5.age = 4
    t6.age = 3
    t7 = trobble.Trobble('7', 'male')
    t8 = trobble.Trobble('8', 'female')
    t7.age = 5
    t8.age = 6
    t9 = trobble.Trobble('9', 'other')
    tests = [((t1, t2, 'aa'), None),
             ((t3, t4, 'aa'), None),
             ((t5, t6, 'aa'), None),
             ((t7, t8, 'aa'), trobble.Trobble('aa', 'male')),
             ((t8, t7, 'aa'), trobble.Trobble('aa', 'female')),
             ((t8, t9, 'aa'), None),
             ((t9, t8, 'aa'), None),
             ((t7, t9, 'aa'), None),
             ((t9, t7, 'aa'), None)
             ]
    generic_tester(trobble.mate, tests)

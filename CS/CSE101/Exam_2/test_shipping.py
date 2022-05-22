# -*- coding: utf-8 -*-
import collections
import io
from contextlib import redirect_stdout
import shipping

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

def ordered_print_tester(f, args):
    """Return the printed output."""
    stream = io.StringIO()
    with redirect_stdout(stream):
        f(*args)
    return stream.getvalue()

#E1
def test_is_before():
    w1 = shipping.Transfer('Rotterdam', (21, 6))
    tests1 = [
             (((21,6),), True),
             (((22,6),), True),
             (((19,7),), True),
             ]
    generic_tester(w1.is_before, tests1)
    tests2 = [
             (((21,6),), True),
             (((22,6),), False),
             (((19,7),), False),
             ]
    generic_tester(w1.is_after, tests2)

#E2
def test_str():
    w1 = shipping.Transfer('Havana', (19,12))
    w2 = shipping.Transfer('Panama', (22,12))
    w3 = shipping.Transfer('Trez Hir', (11,10))
    w4 = shipping.Transfer('St-Germain-en-Laye', (31,12))
    funcs = [
            w1.__str__,
            w2.__str__,
            w3.__str__,
            w4.__str__,
            ]
    expes = [
            'Havana = 19/12',
            'Panama = 22/12',
            'Trez Hir = 11/10',
            'St-Germain-en-Laye = 31/12',
            ]
    generic_value_tester(funcs, expes)

#E3
def test_from_string():
    def tfun(pts):
        w = shipping.transfer_from_string(pts)
        return w.name, w.date
    tests = [
            (('Paris = 8/15',), ('Paris', (8,15))),
            (('Dole = 10/12',), ('Dole', (10,12))),
            (('Hamburg = 1/10',), ('Hamburg', (1,10))),
            (('Minsk = 31/12',), ('Minsk', (31,12))),
            ]
    generic_tester(tfun, tests)

#E4
def test_has_route():
    stops1 = [shipping.Transfer('Porto', (29, 7)),
              shipping.Transfer('Le Havre', (3, 8)),
              shipping.Transfer('Rotterdam', (5, 8))]
    t1 = shipping.Ship('Speed Demon', stops1)
    tests1 = [
             (('Le Havre', 'Rotterdam'), ((3, 8), (5, 8))),
             (('Rotterdam', 'Le Havre'), None),
             (('Porto', 'Napoli'), None),
             (('Napoli', 'Rotterdam'), None)
             ]
    generic_tester(t1.has_route, tests1)
    stops2 = [shipping.Transfer('A', (0,0)),
              shipping.Transfer('B', (1,1)),
              shipping.Transfer('C', (2,2)),
              shipping.Transfer('D', (3,3))]
    t2 = shipping.Ship('Ship', stops2)
    tests2 = [
             (('A', 'B'), ((0,0),(1,1))),
             (('C', 'D'), ((2,2),(3,3))),
             (('B','A'), None),
             (('D','C'), None),
             (('A','E'), None),
             (('E','B'), None),
             ]
    generic_tester(t2.has_route, tests2)

#E5
def test_transporter():
    stops = [shipping.Transfer('Porto', (29, 7)),
              shipping.Transfer('Le Havre', (3, 8)),
              shipping.Transfer('Rotterdam', (5, 8))]
    t1 = shipping.Ship('Speed Demon', stops)
    t2 = shipping.Ship('Short Demon', stops[:-1])
    def tfun1(t):
        maersk = shipping.Transporter('Maersk')
        maersk.add_ship(t)
        return sorted(maersk.ships), maersk.passages
    tests1 = [
             ((t1,), (['Speed Demon'], {'Porto': ['Speed Demon'], 'Le Havre': ['Speed Demon'], 'Rotterdam': ['Speed Demon']})),
             ((t2,), (['Short Demon'], {'Porto': ['Short Demon'], 'Le Havre': ['Short Demon']})),
             ]
    generic_tester(tfun1, tests1)
    def tfun2(t1, t2):
        maersk = shipping.Transporter('maersk')
        maersk.add_ship(t1)
        maersk.add_ship(t2)
        return sorted(maersk.ships), maersk.passages
    tests2 = [
             ((t1,t2), (['Short Demon', 'Speed Demon'], {'Porto': ['Speed Demon', 'Short Demon'], 'Le Havre': ['Speed Demon', 'Short Demon'], 'Rotterdam': ['Speed Demon']})),
             ]
    generic_tester(tfun2, tests2)
    def tfun3(t1, t2):
        maersk = shipping.Transporter('maersk')
        maersk.add_ship(t1)
        maersk.add_ship(t2)
        t = maersk.remove_ship(t1.name)
        return repr(t), sorted(maersk.ships), maersk.passages
    tests3 = [
             ((t2,t1), ('Ship(\'Short Demon\', ["Transfer(\'Porto\', (29, 7))", "Transfer(\'Le Havre\', (3, 8))"])', ['Speed Demon'], {'Porto': ['Speed Demon'], 'Le Havre': ['Speed Demon'], 'Rotterdam': ['Speed Demon']})),
             ((t1,t2), ('Ship(\'Speed Demon\', ["Transfer(\'Porto\', (29, 7))", "Transfer(\'Le Havre\', (3, 8))", "Transfer(\'Rotterdam\', (5, 8))"])', ['Short Demon'], {'Porto': ['Short Demon'], 'Le Havre': ['Short Demon']})),
             ]
    generic_tester(tfun3, tests3)

#E6
def test_load_ships():
    def tfun1():
        maersk = shipping.Transporter('maersk')
        maersk.load_ships_from_file('shipdata.txt')
        return sorted(maersk.ships), maersk.passages
    def tfun2():
        o = shipping.Transporter('O')
        o.load_ships_from_file('testships.txt')
        return sorted(o.ships), o.passages
    funcs = [tfun1, tfun2]
    expes = [
             (['Alpha', 'Beta', 'Fat Crocodile', 'Penguino'], {'Napoli': ['Alpha', 'Beta'], 'Barcelona': ['Alpha'], 'Marseille': ['Alpha', 'Beta'], 'Panama': ['Penguino', 'Fat Crocodile'], 'San Diego': ['Penguino', 'Fat Crocodile'], 'Vancouver': ['Penguino', 'Fat Crocodile'], 'Porto': ['Fat Crocodile']}),
             (['t1', 't2', 't3'],
              {'s1': ['t1', 't3'],
               's2': ['t1', 't2'],
               's3': ['t1'],
               's4': ['t1', 't2'],
               's5': ['t3']})
            ]
    generic_value_tester(funcs, expes)

#E7
def test_find_ships():
    def tfun1(p):
        maersk = shipping.Transporter('maersk')
        maersk.load_ships_from_file('shipdata.txt')
        return maersk.find_ships(p)
    apples = shipping.Container('Apples', shipping.Transfer('Napoli', (28,2)), shipping.Transfer('Marseille', (11,3)))
    bananas = shipping.Container('Bananas', shipping.Transfer('Napoli', (27,2)), shipping.Transfer('Barcelona', (10,3)))
    carrots = shipping.Container('Carrots', shipping.Transfer('Barcelona', (1,1)), shipping.Transfer('Panama', (31,12)))
    dumplings = shipping.Container('Dumplings', shipping.Transfer('Panama', (31,8)), shipping.Transfer('San Diego', (30,9)))
    tests1 = [
             ((apples,), {'Beta', 'Alpha'}),
             ((bananas,), {'Alpha'}),
             ((carrots,), set()),
             ((dumplings,), {'Fat Crocodile', 'Penguino'}),
             ]
    generic_tester(tfun1, tests1)
    def tfun2(p):
        o = shipping.Transporter('maersk')
        o.load_ships_from_file('testships.txt')
        return o.find_ships(p)
    p1 = shipping.Container('p1', shipping.Transfer('s2', (10,4)), shipping.Transfer('s4', (10,10)))
    p2 = shipping.Container('p1', shipping.Transfer('s2', (5,8)), shipping.Transfer('s4', (10,10)))
    p3 = shipping.Container('p1', shipping.Transfer('s2', (10,4)), shipping.Transfer('s4', (10,8)))
    p4 = shipping.Container('p1', shipping.Transfer('s2', (10,6)), shipping.Transfer('s4', (10,8)))
    tests2 = [
             ((p1,), {'t1', 't2'}),
             ((p2,), {'t1', 't2'}),
             ((p3,), set()),
             ((p4,), set()),
             ]
    generic_tester(tfun2, tests2)

#E8
def test_best_ship():
    def tfun1(p):
        maersk = shipping.Transporter('maersk')
        maersk.load_ships_from_file('shipdata.txt')
        return maersk.best_ship(p)
    apples = shipping.Container('Apples', shipping.Transfer('Napoli', (28,2)), shipping.Transfer('Marseille', (11,3)))
    bananas = shipping.Container('Bananas', shipping.Transfer('Napoli', (27,2)), shipping.Transfer('Barcelona', (10,3)))
    dumplings = shipping.Container('Dumplings', shipping.Transfer('Panama', (31,8)), shipping.Transfer('San Diego', (30,9)))
    tests1 = [
             ((apples,), 'Beta'),
             ((bananas,), 'Alpha'),
             ((dumplings,), 'Fat Crocodile'),
             ]
    generic_tester(tfun1, tests1)
    def tfun2(p):
        o = shipping.Transporter('maersk')
        o.load_ships_from_file('testships.txt')
        return o.best_ship(p)
    p1 = shipping.Container('p1', shipping.Transfer('s2', (4,10)), shipping.Transfer('s4', (10,10)))
    p3 = shipping.Container('p1', shipping.Transfer('s2', (4,10)), shipping.Transfer('s4', (8,10)))
    tests2 = [
             ((p1,), 't1'),
             ((p3,), 't1'),
             ]
    generic_tester(tfun2, tests2)




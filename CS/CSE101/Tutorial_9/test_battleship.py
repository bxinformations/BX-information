# -*- coding: utf-8 -*-
import collections
import io
from contextlib import redirect_stdout
import battleship

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

def ordered_print_tester(f, args):
    """Return the printed output."""
    stream = io.StringIO()
    with redirect_stdout(stream):
        f(*args)
    return stream.getvalue()

def ship1():
    s1 = battleship.Ship('Destroyer', {(1,2), (2,3)})
    return s1.__dict__

def ship2():
    s1 = battleship.Ship('Destroyer', {(10,22), (22,32)})
    return s1.__dict__

def ship_eq1():
    s1 = battleship.Ship('Destroyer', {(1,2), (2,3)})
    s2 = battleship.Ship('Destroyer', {(1,2), (2,3)})
    return s1 == s2

def ship_eq2():
    s1 = battleship.Ship('Destroyer', {(1,2), (2,3)})
    s2 = battleship.Ship('Destroyer2', {(1,2), (2,3)})
    return s1 == s2

def ship_eq3():
    s1 = battleship.Ship('Destroyer', {(1,2), (2,3)})
    s2 = battleship.Ship('Destroyer', {(12,2), (2,3)})
    return s1 == s2

def ship_eq4():
    s1 = battleship.Ship('Destroyer', {(1,2), (2,3)})
    s1.hits = {(1,2)}
    s2 = battleship.Ship('Destroyer', {(1,2), (2,3)})
    return s1 == s2

def afloat1():
    s1 = battleship.Ship('Destroyer', {(1,2), (2,3)})
    return s1.is_afloat()

def afloat2():
    s1 = battleship.Ship('Destroyer', {(1,2), (2,3)})
    s1.hits = {(1,2), (2,3)}
    return s1.is_afloat()

def test_ship():
    f = [ship1, ship2, ship_eq1, ship_eq2, ship_eq3, ship_eq4, afloat1, afloat2]
    v = [{'hits': set(), 'name': 'Destroyer', 'positions': {(1, 2), (2, 3)}},
         {'hits': set(), 'name': 'Destroyer', 'positions': {(10, 22), (22, 32)}},
         True, False, False, False, True, False]
    generic_value_tester(f, v)

def grid1():
    return battleship.Grid(10,11).__dict__

def grid2():
    g = battleship.Grid(100,110)
    g.ships.append(battleship.Ship('Destroyer', {(1,2),(2,2)}))
    return g.__dict__

def grid3():
    g = battleship.Grid(2,3)
    g.misses.add((2,3))
    return g.__dict__

def test_grid():
    f = [grid1, grid2, grid3]
    v = [{'misses': set(), 'ships': [], 'sizex': 10, 'sizey': 11},
         {'misses': set(), 'ships': [battleship.Ship('Destroyer', {(1,2),(2,2)})], 'sizex': 100, 'sizey': 110},
         {'misses': {(2, 3)}, 'ships': [], 'sizex': 2, 'sizey': 3}
        ]
    generic_value_tester(f, v)

def load_helper(filename):
    return battleship.load_grid_from_file(filename).__dict__

def test_load():
    tests = {('grid1.grd',): {'sizex': 5, 'sizey': 5, 'ships': [battleship.Ship('Destroyer', {(1, 2), (2, 2)})], 'misses': set()},
             ('grid2.grd',):  {'sizex': 10, 'sizey': 10, 'ships': [battleship.Ship('Carrier', {(2, 5), (2, 6), (2, 3), (2, 4), (2, 2)}), battleship.Ship('Battleship', {(3, 8), (2, 8), (5, 8), (4, 8)}), battleship.Ship('Cruiser', {(4, 5), (5, 5), (6, 5)}), battleship.Ship('Submarine', {(8, 9), (9, 9), (7, 9)}), battleship.Ship('Destroyer', {(1, 8), (1, 9)})], 'misses': set()}}
    generic_tester(load_helper, tests)

def miss():
    s = battleship.Ship('', {(1,1)})
    res = s.shoot_at_ship((0,0))
    return (res, s.__dict__)

def hit():
    s = battleship.Ship('', {(1,1), (1,2)})
    res = s.shoot_at_ship((1,1))
    return (res, s.__dict__)

def kill():
    s = battleship.Ship('', {(1,1), (1,2)})
    s.shoot_at_ship((1,1))
    res = s.shoot_at_ship((1,2))
    return (res, s.__dict__)

def double_shot():
    s = battleship.Ship('', {(1,1), (1,2)})
    s.shoot_at_ship((1,1))
    res = s.shoot_at_ship((1,1))
    return (res, s.__dict__)

def test_shoot_ship():
    f = [miss, hit, double_shot, kill]
    v = [('MISS', {'hits': set(), 'positions': {(1, 1)}, 'name': ''}),
         ('HIT', {'hits': {(1, 1)}, 'positions': {(1, 1), (1,2)}, 'name': ''}),
         ('MISS', {'hits': {(1, 1)}, 'positions': {(1, 1), (1,2)}, 'name': ''}),
         ('DESTROYED', {'hits': {(1, 2), (1, 1)}, 'positions': {(1, 2), (1, 1)}, 'name': ''})]
    generic_value_tester(f, v)

def miss_ships():
    g = battleship.load_grid_from_file('grid1.grd')
    res = g.shoot((1,1))
    return (res, g.__dict__)

def hit_ship():
    g = battleship.load_grid_from_file('grid1.grd')
    res = g.shoot((1,2))
    return (res, g.__dict__)

def sink_ship():
    g = battleship.load_grid_from_file('grid1.grd')
    g.shoot((2,2))
    res = g.shoot((1,2))
    return (res, g.__dict__)

def test_shoot_grid():
    hit_once = battleship.Ship('Destroyer', {(1, 2), (2, 2)})
    hit_once.hits = {(1,2)}
    hit_twice = battleship.Ship('Destroyer', {(1, 2), (2, 2)})
    hit_twice.hits = {(1,2), (2,2)}
    f = [miss_ships, hit_ship, sink_ship]
    v = [(('MISS', None), {'sizex': 5, 'sizey': 5, 'ships': [battleship.Ship('Destroyer', {(1, 2), (2, 2)})], 'misses': {(1, 1)}}),
         (('HIT', None), {'sizex': 5, 'sizey': 5, 'ships': [hit_once], 'misses': set()}),
         (('DESTROYED', hit_twice), {'sizex': 5, 'sizey': 5, 'ships': [hit_twice], 'misses': set()})
        ]
    generic_value_tester(f, v)

def no_shot():
    g = battleship.load_grid_from_file('grid1.grd')
    b = battleship.BlindGrid(g)
    return b.__dict__

def only_miss():
    g = battleship.load_grid_from_file('grid1.grd')
    g.shoot((1,1))
    b = battleship.BlindGrid(g)
    return b.__dict__

def one_hit():
    g = battleship.load_grid_from_file('grid1.grd')
    g.shoot((1,1))
    g.shoot((1,2))
    b = battleship.BlindGrid(g)
    return b.__dict__

def two_hits():
    g = battleship.load_grid_from_file('grid1.grd')
    g.shoot((1,1))
    g.shoot((1,2))
    g.shoot((2,2))
    b = battleship.BlindGrid(g)
    return b.__dict__

def test_blind():
    sunk = battleship.Ship('Destroyer', {(1, 2), (2, 2)})
    sunk.hits = {(1,2), (2,2)}
    f = [no_shot, only_miss, one_hit, two_hits]
    v = [{'sizey': 5, 'sunken_ships': [], 'misses': set(), 'hits': set(), 'sizex': 5},
         {'sizey': 5, 'sunken_ships': [], 'misses': {(1, 1)}, 'hits': set(), 'sizex': 5},
         {'sizey': 5, 'sunken_ships': [], 'misses': {(1, 1)}, 'hits': {(1, 2)}, 'sizex': 5},
         {'sizey': 5, 'sunken_ships': [sunk], 'misses': {(1, 1)}, 'hits': {(1, 2), (2, 2)}, 'sizex': 5}
        ]
    generic_value_tester(f, v)

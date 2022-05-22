# -*- coding: utf-8 -*-
import collections
import io
from contextlib import redirect_stdout
import life

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
                print('FAILED (ran "{}", expected "{}", got "{}")'.format(
                        function.__name__, expected_result, real_out))
            else:
                passed += 1
                print('passed')
        except Exception as e:
            failed += 1
            print('FAILED with exception "{}" when running "{}".'.format(
                    e, function.__name__))
    print('{} tests run, {} passed, {} failed'.format(
            counter,
            passed,
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


def point_repr():
    p = life.Point(0,1)
    return repr(p)

def test_repr():
    functions = [point_repr]
    values = ['Point(0, 1)']
    generic_value_tester(functions, values)

def eq1():
    p1 = life.Point(0,0)
    p2 = life.Point(0,0)
    return p1 == p2
    
def eq2():
    p1 = life.Point(1,0)
    p2 = life.Point(0,0)
    return p1 == p2

def eq3():
    p1 = life.Point(0,1)
    p2 = life.Point(0,0)
    return p1 == p2

def eq4():
    p1 = life.Point(0,1)
    p2 = life.Point(1,0)
    return p1 == p2

def hash1():
    for i in range(10):
        for j in range(10):
            if hash(life.Point(i,j)) != hash(life.Point(i,j)):
                raise Exception('Hash has to be equal for same input. Try Point({},{}).'.format(i,j))
    return None

def hash2():
    values = set()
    for i in range(10):
        for j in range(10):
            values.add(hash(life.Point(i,j)))
    if len(values) <= 10:
        raise Exception('__hash__() gives only {} values on the 10x10 grid! Produce more different values!'.format(len(values)))
    return None
        
def test_hashing():    
    functions = [eq1, eq2, eq3, eq4, hash1, hash2]
    values = [True, False, False, False, None, None]
    generic_value_tester(functions, values)

def generic_permutation_tester(l1, l2):
    """Return True iff l2 is a permutation of l1."""
    seen = [ False for x in l2 ]
    for i in range(len(seen)):
        seen[i] = (l2[i] in l1)
    if False in seen:
        return False
    seen = [ False for x in l1 ]
    for i in range(len(seen)):
        seen[i] = (l1[i] in l2)
    return (False not in seen)

def textView_print(x, y , points):
    b = life.Board(x,y,points)
    t = life.TextView(b)
    t.show()

def test_show():
    """Test santa.print_dictionary."""
    raw_tests = [
                 ((5,5,{life.Point(1,2), life.Point(2,2), life.Point(3,2)}),
                  """ooooooo
o     o
o     o
o XXX o
o     o
o     o
ooooooo
"""),
                   ((40,40,{life.Point (19, 20),
                            life.Point (20, 19),
                            life.Point (20, 20),
                            life.Point (20, 21),
                            life.Point (21, 19)}),
                    """oooooooooooooooooooooooooooooooooooooooooo
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                    XX                  o
o                   XX                   o
o                    X                   o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
o                                        o
oooooooooooooooooooooooooooooooooooooooooo
""")
                ]
    tests = [((textView_print,args),res) 
               for (args,res) in raw_tests ]
    generic_tester(ordered_print_tester, tests)    
    
def neighbors1():
    return life.Point(5,5).get_neighbors()

def neighbors2():
    return life.Point(-10,100).get_neighbors()

def legal1():
    b = life.Board(5,5,set())
    return b.is_legal(life.Point(2,2))

def legal2():
    b = life.Board(5,5,set())
    return b.is_legal(life.Point(0,2))

def legal3():
    b = life.Board(5,5,set())
    return b.is_legal(life.Point(2,0))

def legal4():
    b = life.Board(5,5,set())
    return b.is_legal(life.Point(4,4))

def legal5():
    b = life.Board(5,5,set())
    return b.is_legal(life.Point(5,2))

def legal6():
    b = life.Board(5,5,set())
    return b.is_legal(life.Point(2,5))

def legal7():
    b = life.Board(0,5,set())
    return b.is_legal(life.Point(0,0))

def legal8():
    b = life.Board(5,5,set())
    return b.is_legal(life.Point(-1,4))

def test_neighbors():
    functions = [neighbors1, neighbors2, legal1, legal2, legal3, legal4, legal5, legal6, legal7, legal8]
    values = [{life.Point (4, 4),
               life.Point (4, 5),
               life.Point (4, 6),
               life.Point (5, 4),
               life.Point (5, 6),
               life.Point (6, 4),
               life.Point (6, 5),
               life.Point (6, 6)},
              {life.Point (-10, 101),
               life.Point (-10, 99),
               life.Point (-11, 100),
               life.Point (-11, 101),
               life.Point (-11, 99),
               life.Point (-9, 100),
               life.Point (-9, 101),
               life.Point (-9, 99)},
               True, True, True, True, False, False, False, False
              ]
    generic_value_tester(functions, values)

def live1():
    b = life.Board(5,5, {life.Point(1,2), life.Point(2,2), life.Point(3,2)})
    return b.number_live_neighbors(life.Point(2,2))
    
def live2():
    b = life.Board(5,5, {life.Point(1,2), life.Point(2,2), life.Point(3,2)})
    return b.number_live_neighbors(life.Point(1,2))
    
def live3():
    b = life.Board(5,5, {life.Point(1,2), life.Point(2,2), life.Point(3,2)})
    return b.number_live_neighbors(life.Point(0,2))
    
def live4():
    b = life.Board(5,5, {life.Point(1,2), life.Point(2,2), life.Point(3,2)})
    return b.number_live_neighbors(life.Point(0,0))
    
def live5():
    b = life.Board(5,5, {life.Point(1,2), life.Point(2,2), life.Point(3,2)})
    return b.number_live_neighbors(life.Point(10,10))
    
def test_number_live_neighbors():
    functions = [live1, live2, live3, live4, live5]
    values = [2,1,1,0,0]
    generic_value_tester(functions, values)
    
def next_step1():
    b = life.Board(5,5,{life.Point(1,2),life.Point(2,2),life.Point(3,2)})
    b.next_step()
    return b.points

def next_step2():
    b = life.Board(5,5,{life.Point(1,0),life.Point(2,0),life.Point(3,0)})
    b.next_step()
    return b.points

def test_next_step():
    functions = [next_step1, next_step2]
    values = [{life.Point(2,1),life.Point(2,2),life.Point(2,3)},
              {life.Point (2, 1), life.Point (2, 0)}]
    generic_value_tester(functions, values)
    
def load_blinker_size():
    b1 = life.Board(1,1,set())
    b1.load_from_file('blinker.lf')
    b2 = life.Board(5,5,{life.Point(1,2), life.Point(2,2), life.Point(3,2)})
    return b1.sizex == b2.sizex and b1.sizey == b2.sizey

def load_blinker_points():
    b1 = life.Board(1,1,set())
    b1.load_from_file('blinker.lf')
    b2 = life.Board(5,5,{life.Point(1,2), life.Point(2,2), life.Point(3,2)})
    return b1.points == b2.points

def load_blinker():
    b1 = life.Board(1,1,set())
    b1.load_from_file('blinker.lf')
    b2 = life.Board(5,5,{life.Point(1,2), life.Point(2,2), life.Point(3,2)})
    return b1.points == b2.points and b1.sizex == b2.sizex and b1.sizey == b2.sizey

def load_twice():
    b1 = life.Board(1,1,set())
    b1.load_from_file('smallCross.lf')
    b1.load_from_file('blinker.lf')
    b2 = life.Board(5,5,{life.Point(1,2), life.Point(2,2), life.Point(3,2)})
    return b1.points == b2.points and b1.sizex == b2.sizex and b1.sizey == b2.sizey

def test_load():
    functions = [load_blinker_size, load_blinker_points, load_blinker, load_twice]
    values = [True, True, True, True]
    generic_value_tester(functions, values)

def toggle_on():
    b1 = life.Board(5,5,{life.Point(1,2), life.Point(2,2), life.Point(3,2)})
    b1.toggle_point(3,3)
    return b1.points == {life.Point(1,2), life.Point(2,2), life.Point(3,2), life.Point(3,3)}

def toggle_off():
    b1 = life.Board(5,5,{life.Point(1,2), life.Point(2,2), life.Point(3,2)})
    b1.toggle_point(2,2)
    return b1.points == {life.Point(1,2), life.Point(3,2)}

def test_toggle():
    functions = [toggle_on, toggle_off]
    values = [True, True]
    generic_value_tester(functions, values)

def save():
    b2 = life.Board(5,5,{life.Point(1,2), life.Point(2,2), life.Point(3,2)})
    b2.save_to_file('temp_test')
    b1 = life.Board(6,6,set())
    b1.load_from_file('temp_test')
    return  b1.points == b2.points and b1.sizex == b2.sizex and b1.sizey == b2.sizey

def test_save():
    functions = [save]
    values = [True]
    generic_value_tester(functions, values)


# -*- coding: utf-8 -*-
import collections
import io
from contextlib import redirect_stdout
import index

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

def generic_output_file_tester(outputfile, ref_outputfile):
    """Compare the specified output file to the reference file"""
    m = 'Comparing files {} and {}: '.format(outputfile, ref_outputfile)
    passed = True
    try:
        ref_line_count = file_line_count(ref_outputfile)
        f_line_count = file_line_count(outputfile)
        if (ref_line_count != f_line_count):
            print(m + 'FAILED: wrong number of lines: {} {}, but {} {}'.format(
                    outputfile, f_line_count, ref_outputfile, ref_line_count))
            passed = False
        else:
            with open(outputfile) as ofile, open(ref_outputfile) as rfile:
                original = ofile.readlines()
                reference = rfile.readlines()
                for i in range(len(original)):
                    if original[i] != reference[i]:
                        print(m + ('FAILED in output line {} of {}: '
                            + 'did not find "{}" in reference file\n').format(
                                i, outputfile, original[i].strip()))
                        passed = False
                        break
        return passed
    except Exception as e:
        print(m + 'FAILED with exception {}.'.format(e))

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

def print_tester(f, args):
    """Return the printed output."""
    stream = io.StringIO()
    with redirect_stdout(stream):
        f(*args)
    return stream.getvalue()

def tree1():
    r = index.Node(1, 2, index.Node(3, 4, None, None), index.Node(5, 6, None, None))
    return str(r)

def tree2():
    r = index.Node(1, 2, index.Node(3, 4, None, None), index.Node(5, 6, None, None))
    return repr(r)

def tree3():
    r = index.example_bst()
    return repr(r)

def test_example_bst():
    f = [tree1, tree2, tree3]
    v = ['Tree node; key: 1, value: 2',
         'Node(1, 2, Node(3, 4, None, None), Node(5, 6, None, None))',
         "Node(8, 'Eight', Node(4, 'Four', Node(3, 'Three', None, None), Node(6, 'Six', None, Node(7, 'Seven', None, None))), Node(10, 'Ten', None, Node(14, 'Fourteen', Node(13, 'Thirteen', None, None), None)))"
        ]
    generic_value_tester(f, v)

def search1():
    r = index.example_bst()
    return (r.search(8), r.search(4), r.search(2))

def search2():
    r = index.Node(1, 2, None, index.Node(3, 4, None, index.Node(5, 6, None, index.Node(7, 8, None, None))))
    return (r.search(1), r.search(7), r.search(0), r.search(9))

def search3():
    r = index.Node(7, 8, index.Node( 5, 6, index.Node(3, 4, index.Node(1, 2, None, None), None), None), None)
    return (r.search(1), r.search(7), r.search(0), r.search(9))

def test_search():
    f = [search1, search2, search3]
    v = [('Eight', 'Four', None), (2, 8, None, None), (2, 8, None, None)]
    generic_value_tester(f, v)

def print1():
    r = index.example_bst()
    return print_tester(r.print_in_order, ())

def print2():
    r = index.Node(1, 2, None, index.Node(3, 4, None, index.Node(5, 6, None, index.Node(7, 8, None, None))))
    return print_tester(r.print_in_order, ())

def print3():
    r = index.Node(7, 8, index.Node( 5, 6, index.Node(3, 4, index.Node(1, 2, None, None), None), None), None)
    return print_tester(r.print_in_order, ())

def test_print():
    f = [print1, print2, print3]
    v = ['3: Three\n4: Four\n6: Six\n7: Seven\n8: Eight\n10: Ten\n13: Thirteen\n14: Fourteen\n',
         '1: 2\n3: 4\n5: 6\n7: 8\n',
         '1: 2\n3: 4\n5: 6\n7: 8\n'
        ]
    generic_value_tester(f, v)

def add1():
    r = index.Node('quick', [1], None, None)
    r.add('the', 1)
    r.add('fox', 1)
    r.add('fox', 1)
    r.add('fox', 2)
    return repr(r)

def add2():
    r = index.Node(1, 2, None, index.Node(3, 4, None, index.Node(5, 6, None, index.Node(7, 8, None, None))))
    r.add(9, 10)
    return repr(r)

def add3():
    r = index.Node(1, 2, None, index.Node(3, 4, None, index.Node(5, 6, None, index.Node(7, 8, None, None))))
    r.add(6, 7)
    return repr(r)

def add4():
    r = index.Node(7, 8, index.Node( 5, 6, index.Node(3, 4, index.Node(1, 2, None, None), None), None), None)
    r.add(0, 1)
    return repr(r)

def add5():
    r = index.Node(7, 8, index.Node( 5, 6, index.Node(3, 4, index.Node(1, 2, None, None), None), None), None)
    r.add(4, 5)
    return repr(r)

def add6():
    r = index.Node(7, 8, index.Node( 5, 6, index.Node(3, 4, index.Node(1, 2, None, None), None), None), None)
    r.add(4, 5)
    r.add(4, 5)
    r.add(4, 6)
    return repr(r)

def test_add():
    f = [add1, add2, add3, add4, add5, add6]
    v = ["Node('quick', [1], Node('fox', [1, 2], None, None), Node('the', [1], None, None))",
         'Node(1, 2, None, Node(3, 4, None, Node(5, 6, None, Node(7, 8, None, Node(9, [10], None, None)))))',
         'Node(1, 2, None, Node(3, 4, None, Node(5, 6, None, Node(7, 8, Node(6, [7], None, None), None))))',
         'Node(7, 8, Node(5, 6, Node(3, 4, Node(1, 2, Node(0, [1], None, None), None), None), None), None)',
         'Node(7, 8, Node(5, 6, Node(3, 4, Node(1, 2, None, None), Node(4, [5], None, None)), None), None)',
         'Node(7, 8, Node(5, 6, Node(3, 4, Node(1, 2, None, None), Node(4, [5, 6], None, None)), None), None)'
        ]
    generic_value_tester(f, v)

def bstindex1():
    r = index.construct_bst_for_indexing('foxdog.txt')
    return repr(r)

def bstindex2():
    r = index.construct_bst_for_indexing('test1.txt')
    return repr(r)

def bstindex3():
    r = index.construct_bst_for_indexing('test2.txt')
    return repr(r)

def bstindex4():
    r = index.construct_bst_for_indexing('test3.txt')
    return repr(r)

def test_bst_index():
    f = [bstindex1, bstindex2, bstindex3, bstindex4]
    v = ["Node('the', [1, 2], Node('quick', [1], Node('brown', [1, 3], None, Node('fox', [1], Node('dog', [3], None, None), Node('jumps', [2], None, Node('over', [2], Node('lazy', [3], None, None), None)))), None), None)",
         "Node('a', [1], None, Node('b', [2], None, Node('c', [3], None, Node('d', [4], None, Node('e', [5], None, None)))))",
         "Node('e', [1], Node('d', [2], Node('c', [3], Node('b', [4], Node('a', [5], None, None), None), None), None), None)",
         "Node('d', [1, 2], Node('b', [1, 2], Node('a', [1, 2], None, None), Node('c', [1, 2], None, None)), Node('f', [1, 3], Node('e', [1, 3], None, None), Node('g', [1, 3], None, None)))"
        ]
    generic_value_tester(f, v)

def index1():
    r = index.construct_bst_for_indexing('foxdog.txt')
    r.write_in_order('test.txt')
    return generic_output_file_tester('test.txt', 'test.txt.ref')

def index2():
    index.generate_index('foxdog.txt')
    return generic_output_file_tester('foxdog.txt.index', 'foxdog.txt.index.ref')

def index3():
    index.generate_index('test3.txt')
    return generic_output_file_tester('test3.txt.index', 'test3.txt.index.ref')

def test_index():
    f = [index1, index2, index3]
    v = [True, True, True]
    generic_value_tester(f, v)

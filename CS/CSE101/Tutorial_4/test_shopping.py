# -*- coding: utf-8 -*-
import shopping
import io
import collections
from contextlib import redirect_stdout

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

def unordered_print_tester(f, args):
    """Return a counter object counting the lines printed
    to standard output by the function f with arguments args."""
    stream = io.StringIO()
    with redirect_stdout(stream):
        f(*args)
    return collections.Counter(stream.getvalue().split('\n')[:-1])

def test_print_recipe():
    raw_tests = [
                 (({'milk':19, 'eggs':2},),
                  ['milk: 19', 'eggs: 2']),
                 (({'cheese':10,
                    'another cheese':11},),
                  ['cheese: 10',
                   'another cheese: 11']),
                ]
    tests = [((shopping.print_recipe,args),collections.Counter(res))
               for (args,res) in raw_tests ]
    generic_tester(unordered_print_tester, tests)

def test_read_recipe():
    tests = [(['flan.txt'],{'egg': 2, 'cornstarch': 50, 'milk': 500, 'vanilla': 15, 'sugar': 150}),
             (['crepes.txt'],{'egg': 4, 'wheat flour': 250, 'butter': 50, 'milk': 50, 'salt': 1}),
             (['madeleines.txt'],{'vanilla': 5, 'butter': 125, 'egg': 2, 'wheat flour': 100, 'sugar': 100, 'yeast': 1, 'almond': 20})]
    generic_tester(shopping.read_recipe, tests)

def test_read_fridge():
    tests = [(['flan.txt'],{'egg': 2, 'cornstarch': 50, 'milk': 500, 'vanilla': 15, 'sugar': 150}),
             (['fridge.txt'],{'salt': 10, 'vanilla': 20, 'sugar': 200, 'egg': 3, 'milk': 800, 'butter': 100, 'cornstarch': 50})]
    generic_tester(shopping.read_fridge, tests)

def test_write_recipe():
    args= ({'egg': 2, 'cornstarch': 50, 'milk': 500, 'vanilla': 15, 'sugar': 150},'test_flan.out')
    generic_output_file_without_order_tester(shopping.write_recipe, args,
                               'test_flan.out','ref_flan.out')

def test_is_cookable():
    tests = [(['crepes.txt','fridge.txt'],False),
             (['flan.txt','fridge.txt'],True),
             (['madeleines.txt','fridge.txt'],False)]
    generic_tester(shopping.is_cookable, tests)

def test_add_recipes():
    tests = [([[]],{}),
             ([[{'a':1}]],{'a':1}),
             ([[{'a':1},{'b':1}]],{'a':1,'b':1}),
             ([[{'a':1},{'a':2}]],{'a':3}),
             ([[{'a':1,'b':2,'c':3},{'b':1,'c':2,'d':3}]],{'a':1,'b':3,'c':5,'d':3}),
             ([[{'a':1,'b':2,'c':3},{'b':1,'c':2,'d':3},{'c':1,'d':2,'e':3}]],{'a':1,'b':3,'c':6,'d':5, 'e':3}),
             ]
    generic_tester(shopping.add_recipes, tests)

def test_create_shopping_list():
    tests = [([[],'fridge.txt'],{}),
             ([['crepes.txt'],'fridge.txt'],{'egg': 1, 'wheat flour': 250}),
             ([['flan.txt'],'fridge.txt'],{}),
             ([['flan.txt','crepes.txt','madeleines.txt'],'fridge.txt'],{'butter': 75, 'egg': 5, 'yeast': 1, 'sugar': 50, 'wheat flour': 350, 'almond': 20})]
    generic_tester(shopping.create_shopping_list, tests)

def test_total_price():
    tests = [([{},'market1.txt'],0),
             ([{'milk':0},'market1.txt'],0),
             ([{'milk':1},'market1.txt'],180),
             ([{'egg':10,'milk':1},'market1.txt'],580),
             ([{'egg':10,'milk':1},'market2.txt'],500),
             ([{'egg':10,'milk':10,'salt':10},'market2.txt'],2350)]
    generic_tester(shopping.total_price, tests)

def test_find_cheapest():
    tests = [([{},['market1.txt']],('market1.txt',0)),
             ([{'butter':1},['market1.txt','market2.txt','market3.txt']],('market1.txt',500)),
             ([{'butter':1},['market2.txt','market3.txt']],('market2.txt',550)),
             ([{'egg':1},['market1.txt','market2.txt','market3.txt']],('market3.txt',30)),
             ([{'butter': 75, 'egg': 5, 'yeast': 1, 'sugar': 50, 'wheat flour': 350, 'almond': 20},['market1.txt','market2.txt','market3.txt']],('market2.txt',144925))
             ]
    generic_tester(shopping.find_cheapest, tests)

def test_distributed_shopping_list():
    tests = [([{},[]],{}),
             ([{'egg':1},['market1.txt','market2.txt','market3.txt']],{'market2.txt': {}, 'market3.txt': {'egg': 1}, 'market1.txt': {}}),
             ([{'egg':1,'milk':1},['market1.txt','market2.txt','market3.txt']],{'market2.txt': {'milk': 1}, 'market3.txt': {'egg': 1}, 'market1.txt': {}}),
             ([{'egg':10,'milk':20,'butter':30,'cornstarch':40},['market1.txt','market2.txt','market3.txt']],{'market2.txt': {'milk': 20}, 'market3.txt': {'egg': 10, 'cornstarch': 40}, 'market1.txt': {'butter': 30}})]
    generic_tester(shopping.distributed_shopping_list, tests)


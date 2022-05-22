# -*- coding: utf-8 -*-
import collections
import io
from contextlib import redirect_stdout
import money

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


def test_account():
    def account_init1():
        a = money.Account('Mia', '0001', 20)
        return a.owner

    def account_init2():
        a = money.Account('Mia', '0001', 20)
        return a.IBAN

    def account_init3():
        a = money.Account('Mia', '0001', 20)
        return a.balance

    def account_str1():
        a = money.Account('Mia', '0001', 20)
        return a.__str__()

    def account_str2():
        a = money.Account('Mila', '1234', 0)
        return a.__str__()

    def account_eq1():
        a1 = money.Account('Mia', '0001', 20)
        return a1 == money.Account('Jim', '0001', 2000)

    def account_eq2():
        a1 = money.Account('Mia', '0001', 20)
        return a1 == money.Account('Mia', '1234', 20)

    functions = [
        account_init1,
        account_init2,
        account_init3,
        account_str1,
        account_str2,
        account_eq1,
        account_eq2,
    ]

    values = ['Mia', '0001', 20,
              'Owner: Mia\nIBAN: 0001\nbalance: 20',
              'Owner: Mila\nIBAN: 1234\nbalance: 0',
              True,
              False]
    generic_value_tester(functions, values)

def test_transactions():
    def account_transactions1():
        a = money.Account('Mia', '0001', 20)
        return a.deposit(20)

    def account_transactions2():
        a = money.Account('Mila', '1234', 0)
        return a.deposit(-1220)

    def account_transactions3():
        a = money.Account('Mila', '1234', 0)
        return a.deposit(0)

    def account_transactions4():
        a = money.Account('Mia', '0001', 20)
        return a.withdraw(20)

    def account_transactions5():
        a = money.Account('Mila', '1234', 0)
        return a.withdraw(20)

    def account_transactions6():
        a = money.Account('Mila', '1234', 0)
        return a.withdraw(-20)

    def account_transactions7():
        a = money.Account('Mila', '1234', 0)
        return a.withdraw(0)

    functions = [
        account_transactions1,
        account_transactions2,
        account_transactions3,
        account_transactions4,
        account_transactions5,
        account_transactions6,
        account_transactions7,
        ]
    values = [(True, 40),
              (False, 'Amount must be positive.'),
              (False, 'Amount must be positive.'),
              (True, 0),
              (False, 'Insufficient funds.'),
              (False, 'Amount must be positive.'),
              (False, 'Amount must be positive.'),
              ]
    generic_value_tester(functions, values)

def test_bank():
    def account_binit1():
        b = money.Bank('CIC')
        return b.name

    def account_binit2():
        b = money.Bank('SG')
        return b.accounts

    def account_bstr1():
        b = money.Bank('CA')
        return str(b)

    def account_bstr2():
        b = money.Bank('BRD')
        b.accounts = {'89876543': money.Account('Mila','8987654343',60)}
        return str(b)

    def account_bstr3():
        b = money.Bank('BRD')
        b.accounts = {'s2': money.Account('B','s2',2),'1': money.Account('A','s1',1)}
        s = str(b)
        b, c = s.split('\n\n')
        a, b = b.split('\n', maxsplit=1)
        return (a, {b,c})

    functions = [account_binit1,account_binit2,account_bstr1, account_bstr2, account_bstr3]
    values = ['CIC',
              {},
              'Bank CA has no accounts.',
              'Bank BRD has the following accounts:\nOwner: Mila\nIBAN: 8987654343\nbalance: 60',
              ('Bank BRD has the following accounts:',  {'Owner: A\nIBAN: s1\nbalance: 1', 'Owner: B\nIBAN: s2\nbalance: 2'})
              ]
    generic_value_tester(functions, values)

def test_operations_account():
    def account_badd1():
        b = money.Bank('CIC')
        return b.open_account('Rita', '0976306916', 0)

    def account_badd2():
        b = money.Bank('CIC')
        b.open_account('Rita', '0976306916', 0)
        return b.open_account('Rita', '0976306916', 0)

    def account_bclose1():
        b = money.Bank('CIC')
        return b.close_account('0976306916')

    def account_bclose2():
        b = money.Bank('CIC')
        b.open_account('Rita', '0976306916', 20)
        return b.close_account('0976306916')

    functions = [account_badd1, account_badd2, account_bclose1, account_bclose2]
    values = [None,
              'IBAN already taken!',
              None,
             20]
    generic_value_tester(functions, values)

def test_transactions_account():
    def account_search1():
        b = money.Bank('SGC')
        a1 = money.Account('Mila', '6492', 30)
        b.open_account(a1.owner, a1.IBAN, a1.balance)
        a2 = money.Account('Mia', '6000', 3000)
        b.open_account(a2.owner, a2.IBAN, a2.balance)
        return b.holds_account('0001')

    def account_search2():
        b = money.Bank('SGC')
        a = money.Account('Noah', '0907', 0)
        b.open_account(a.owner, a.IBAN, a.balance)
        return b.holds_account('0907')

    def account_bdeposit1():
        b = money.Bank('SGC')
        b.open_account('Dominique', '1099306916', 340)
        return b.deposit_to('1099306916', 100)

    def account_bwithdraw2():
        b = money.Bank('SGC')
        b.open_account('Dominique', '1099306916', 30)
        return b.withdraw_from('1099306765', 100)

    functions = [account_search1, account_search2, account_bdeposit1, account_bwithdraw2]
    values = [
        False,
        True,
        (True, 440),
        (False, 'IBAN not found.'),
        ]
    generic_value_tester(functions, values)

def test_create_bank_from_file():
    def banktest1():
        b = money.create_bank_from_file('test1.bnk')
        return b.accounts

    def banktest2():
        b = money.create_bank_from_file('test2.bnk')
        return len(b.accounts)

    functions = [banktest1, banktest2]
    values = [
            {'0975':money.Account('Adi', '0975', 5000)},
            4,
            ]
    generic_value_tester(functions, values)

def test_transfer():
    def transfertest1():
        bank = money.create_bank_from_file('bank1.bnk')
        return bank.transfer('0000', '0009', 2)

    def transfertest2():
        bank = money.create_bank_from_file('bank1.bnk')
        return bank.transfer('0009', '0001', 2)

    def transfertest3():
        bank = money.create_bank_from_file('bank1.bnk')
        return bank.transfer('0009', '0975', 51)

    def transfertest4():
        bank = money.create_bank_from_file('bank1.bnk')
        return bank.transfer('0975', '0900', 5000)

    def transfertest5():
        bank = money.create_bank_from_file('bank1.bnk')
        return bank.transfer('0975', '0900', 0)

    functions = [
            transfertest1,
            transfertest2,
            transfertest3,
            transfertest4,
            transfertest5,
            ]
    values = [
        'Sender not found.',
        'Receiver not found.',
        'Insufficient funds.',
        'Transfer successful.',
        'Amount must be positive.',
        ]
    generic_value_tester(functions, values)

def test_transfer_inter():
    def transfertest1():
        bank = money.create_bank_from_file('bank1.bnk')
        return bank.transfer('0000', '0009', 2)

    def transfertest2():
        bank = money.create_bank_from_file('bank1.bnk')
        return bank.transfer('0009', '0001', 2)

    def transfertest3():
        bank = money.create_bank_from_file('bank1.bnk')
        return bank.transfer('0009', '0975', 51)

    def transfertest4():
        bank = money.create_bank_from_file('bank1.bnk')
        return bank.transfer('0975', '0900', 5000)

    def transfertest5():
        bank = money.create_bank_from_file('bank1.bnk')
        return bank.transfer('0975', '0900', 0)

    def transfertest6():
        bank1 = money.create_bank_from_file('bank1.bnk')
        bank2 = money.create_bank_from_file('bank2.bnk')
        bank1.link_bank(bank2)
        return bank1.transfer('0970', '0000', 200)

    def transfertest7():
        bank1 = money.create_bank_from_file('bank1.bnk')
        bank2 = money.create_bank_from_file('bank2.bnk')
        bank1.link_bank(bank2)
        return bank1.transfer('5503', '9007', 500)

    def transfertest8():
        bank1 = money.create_bank_from_file('bank1.bnk')
        bank2 = money.create_bank_from_file('bank2.bnk')
        bank1.link_bank(bank2)
        return bank1.transfer('9075', '0970', 10)

    functions = [
            transfertest1,
            transfertest2,
            transfertest3,
            transfertest4,
            transfertest5,
            transfertest6,
            transfertest7,
            transfertest8,
            ]
    values = [
        'Sender not found.',
        'Receiver not found.',
        'Insufficient funds.',
        'Transfer successful.',
        'Amount must be positive.',
        'Receiver not found.',
        'Transfer successful.',
        'Sender not found.',
        ]
    generic_value_tester(functions, values)

def test_total_balances():
    def test1():
        banks = []
        for i in ['bank1.bnk','bank2.bnk']:
            banks.append(money.create_bank_from_file(i))
        s = money.total_balances(banks)
        return s

    def test2():
        banks = []
        for i in ['bank1.bnk','bank2.bnk']:
            banks.append(money.create_bank_from_file(i))
        s = money.total_balances(banks)
        return len(s)

    functions = [test1, test2]
    values = [{'Andy': 5400,
              'Iuri': 50,
              'Milo': 591,
              'Adi': 5600,
              'Mila': 8793,
              'Ine': 50,
              'Aline': 234},
              7
              ]
    generic_value_tester(functions, values)

def test_all():
    print('-- test_bank_str()')
    test_bank_str()
    print('-- test_str()')
    test_str()
    print('-- test_transactions_account()')
    test_transactions_account()
    print('-- test_create_bank_from_file()')
    test_create_bank_from_file()
    print('-- test_total_balances()')
    test_total_balances()
    print('-- test_transfer()')
    test_transfer()
    print('-- test_transfer_inter()')
    test_transfer_inter()
    print('-- test_operations_account()')
    test_operations_account()
    print('-- test_transactions()')
    test_transactions()


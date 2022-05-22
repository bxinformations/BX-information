# -*- coding: utf-8 -*-
import collections
import io
from contextlib import redirect_stdout
import per

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

def card_initstr1():
    c = per.Card(0, 0)
    return (c.__dict__, str(c))

def card_initstr2():
    c = per.Card(2, 7)
    return (c.__dict__, str(c))

def card_initstr3():
    c = per.Card(3, 12)
    return (c.__dict__, str(c))

def card_eq1():
    c1 = per.Card(1, 1)
    c2 = per.Card(1, 1)
    return c1 == c2
    
def card_eq2():
    c1 = per.Card(2, 2)
    c2 = per.Card(1, 2)
    return c1 == c2
    
def card_eq3():
    c1 = per.Card(2, 2)
    c2 = per.Card(2, 10)
    return c1 == c2
    
def test_card_init():
    functions = [card_initstr1, card_initstr2, card_initstr3]
    values = [({'rank': 0, 'suit': 0}, 'Two of Clubs'),
              ({'rank': 7, 'suit': 2}, 'Nine of Hearts'),
              ({'rank': 12, 'suit': 3}, 'Ace of Spades'),
              True,
              False,
              False]
    generic_value_tester(functions, values)

def test_card_eq():
    functions = [card_eq1, card_eq2, card_eq3]
    values = [True,
              False,
              False]
    generic_value_tester(functions, values)

def deck_init1():
    d = per.Deck(0)
    return [c.__dict__ for c in d.cards]

def deck_init2():
    d = per.Deck(0)
    return [c.__dict__ for c in d.cards]

def deck_init3():
    d = per.Deck(10)
    return [c.__dict__ for c in d.cards]

def deck_str1():
    d = per.Deck(0)
    return str(d)

def deck_str2():
    d = per.Deck(10)
    return str(d)

def deck_pop1():
    d = per.Deck(0)
    return d.pop().__dict__

def deck_pop2():
    d = per.Deck(8)
    return d.pop().__dict__

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

def deck_shuffle1():
    d1 = per.Deck(10)
    d1.shuffle()
    d2 = per.Deck(10)
    return generic_permutation_tester(d1.cards, d2.cards)

def deck_shuffle2():
    d1 = per.Deck(0)
    d1.shuffle()
    d2 = per.Deck(0)
    return generic_permutation_tester(d1.cards, d2.cards)

def test_deck():
    functions = [deck_init1, deck_init2, deck_init3, deck_str1, deck_str2,
                 deck_pop1, deck_pop2, deck_shuffle1, deck_shuffle2]
    values = [[{'rank': 0, 'suit': 0},
               {'rank': 1, 'suit': 0},
               {'rank': 2, 'suit': 0},
               {'rank': 3, 'suit': 0},
               {'rank': 4, 'suit': 0},
               {'rank': 5, 'suit': 0},
               {'rank': 6, 'suit': 0},
               {'rank': 7, 'suit': 0},
               {'rank': 8, 'suit': 0},
               {'rank': 9, 'suit': 0},
               {'rank': 11, 'suit': 0},
               {'rank': 12, 'suit': 0},
               {'rank': 0, 'suit': 1},
               {'rank': 1, 'suit': 1},
               {'rank': 2, 'suit': 1},
               {'rank': 3, 'suit': 1},
               {'rank': 4, 'suit': 1},
               {'rank': 5, 'suit': 1},
               {'rank': 6, 'suit': 1},
               {'rank': 7, 'suit': 1},
               {'rank': 8, 'suit': 1},
               {'rank': 9, 'suit': 1},
               {'rank': 10, 'suit': 1},
               {'rank': 11, 'suit': 1},
               {'rank': 12, 'suit': 1},
               {'rank': 0, 'suit': 2},
               {'rank': 1, 'suit': 2},
               {'rank': 2, 'suit': 2},
               {'rank': 3, 'suit': 2},
               {'rank': 4, 'suit': 2},
               {'rank': 5, 'suit': 2},
               {'rank': 6, 'suit': 2},
               {'rank': 7, 'suit': 2},
               {'rank': 8, 'suit': 2},
               {'rank': 9, 'suit': 2},
               {'rank': 10, 'suit': 2},
               {'rank': 11, 'suit': 2},
               {'rank': 12, 'suit': 2},
               {'rank': 0, 'suit': 3},
               {'rank': 1, 'suit': 3},
               {'rank': 2, 'suit': 3},
               {'rank': 3, 'suit': 3},
               {'rank': 4, 'suit': 3},
               {'rank': 5, 'suit': 3},
               {'rank': 6, 'suit': 3},
               {'rank': 7, 'suit': 3},
               {'rank': 8, 'suit': 3},
               {'rank': 9, 'suit': 3},
               {'rank': 10, 'suit': 3},
               {'rank': 11, 'suit': 3},
               {'rank': 12, 'suit': 3}],
              [{'rank': 0, 'suit': 0},
               {'rank': 1, 'suit': 0},
               {'rank': 2, 'suit': 0},
               {'rank': 3, 'suit': 0},
               {'rank': 4, 'suit': 0},
               {'rank': 5, 'suit': 0},
               {'rank': 6, 'suit': 0},
               {'rank': 7, 'suit': 0},
               {'rank': 8, 'suit': 0},
               {'rank': 9, 'suit': 0},
               {'rank': 11, 'suit': 0},
               {'rank': 12, 'suit': 0},
               {'rank': 0, 'suit': 1},
               {'rank': 1, 'suit': 1},
               {'rank': 2, 'suit': 1},
               {'rank': 3, 'suit': 1},
               {'rank': 4, 'suit': 1},
               {'rank': 5, 'suit': 1},
               {'rank': 6, 'suit': 1},
               {'rank': 7, 'suit': 1},
               {'rank': 8, 'suit': 1},
               {'rank': 9, 'suit': 1},
               {'rank': 10, 'suit': 1},
               {'rank': 11, 'suit': 1},
               {'rank': 12, 'suit': 1},
               {'rank': 0, 'suit': 2},
               {'rank': 1, 'suit': 2},
               {'rank': 2, 'suit': 2},
               {'rank': 3, 'suit': 2},
               {'rank': 4, 'suit': 2},
               {'rank': 5, 'suit': 2},
               {'rank': 6, 'suit': 2},
               {'rank': 7, 'suit': 2},
               {'rank': 8, 'suit': 2},
               {'rank': 9, 'suit': 2},
               {'rank': 10, 'suit': 2},
               {'rank': 11, 'suit': 2},
               {'rank': 12, 'suit': 2},
               {'rank': 0, 'suit': 3},
               {'rank': 1, 'suit': 3},
               {'rank': 2, 'suit': 3},
               {'rank': 3, 'suit': 3},
               {'rank': 4, 'suit': 3},
               {'rank': 5, 'suit': 3},
               {'rank': 6, 'suit': 3},
               {'rank': 7, 'suit': 3},
               {'rank': 8, 'suit': 3},
               {'rank': 9, 'suit': 3},
               {'rank': 10, 'suit': 3},
               {'rank': 11, 'suit': 3},
               {'rank': 12, 'suit': 3}],
              [{'rank': 11, 'suit': 0},
               {'rank': 12, 'suit': 0},
               {'rank': 10, 'suit': 1},
               {'rank': 11, 'suit': 1},
               {'rank': 12, 'suit': 1},
               {'rank': 10, 'suit': 2},
               {'rank': 11, 'suit': 2},
               {'rank': 12, 'suit': 2},
               {'rank': 10, 'suit': 3},
               {'rank': 11, 'suit': 3},
               {'rank': 12, 'suit': 3}],
              'Two of Clubs, Three of Clubs, Four of Clubs, Five of Clubs, Six of Clubs, Seven of Clubs, Eight of Clubs, Nine of Clubs, Ten of Clubs, Jack of Clubs, King of Clubs, Ace of Clubs, Two of Diamonds, Three of Diamonds, Four of Diamonds, Five of Diamonds, Six of Diamonds, Seven of Diamonds, Eight of Diamonds, Nine of Diamonds, Ten of Diamonds, Jack of Diamonds, Queen of Diamonds, King of Diamonds, Ace of Diamonds, Two of Hearts, Three of Hearts, Four of Hearts, Five of Hearts, Six of Hearts, Seven of Hearts, Eight of Hearts, Nine of Hearts, Ten of Hearts, Jack of Hearts, Queen of Hearts, King of Hearts, Ace of Hearts, Two of Spades, Three of Spades, Four of Spades, Five of Spades, Six of Spades, Seven of Spades, Eight of Spades, Nine of Spades, Ten of Spades, Jack of Spades, Queen of Spades, King of Spades, Ace of Spades',
              'King of Clubs, Ace of Clubs, Queen of Diamonds, King of Diamonds, Ace of Diamonds, Queen of Hearts, King of Hearts, Ace of Hearts, Queen of Spades, King of Spades, Ace of Spades',
              {'rank': 12, 'suit': 3},
              {'rank': 12, 'suit': 3},
              True,
              True]
    generic_value_tester(functions, values)

def player_init1():
    p = per.Player('aA')
    return p.__dict__

def player_init2():
    p = per.Player('Bb')
    return p.__dict__

def player_str1():
    p = per.Player('Aa')
    return str(p)

def player_str2():
    p = per.Player('Aa')
    p.cards = [per.Card(1, 1), per.Card(2, 2)]
    return str(p)

def player_add_card1():
    p = per.Player('A')
    p.add_card(per.Card(0, 0))
    return [ c.__dict__ for c in p.cards ]

def player_add_card2():
    p = per.Player('A')
    p.add_card(per.Card(0, 0))
    p.add_card(per.Card(1, 1))
    p.add_card(per.Card(2, 2))
    return [ c.__dict__ for c in p.cards ]

def player_num_cards1():
    p = per.Player('Aa')
    return p.num_cards()

def player_num_cards2():
    p = per.Player('Aa')
    p.cards = [per.Card(0, 0), per.Card(1, 1)]
    return p.num_cards()

def player_remove_card1():
    p = per.Player('Aa')
    p.cards = [per.Card(0, 0)]
    card = p.remove_card(0)
    return (card.__dict__, [ c.__dict__ for c in p.cards ])

def player_remove_card2():
    p = per.Player('Aa')
    p.cards = [per.Card(0, 0), per.Card(1, 1), per.Card(2, 2)]
    card = p.remove_card(1)
    return (card.__dict__, [ c.__dict__ for c in p.cards ])

def test_player():
    functions = [player_init1, player_init2, player_str1, player_str2,
                 player_add_card1, player_add_card2,
                 player_num_cards1, player_num_cards2,
                 player_remove_card1, player_remove_card2]
    values = [{'name': 'aA', 'cards': []},
              {'name': 'Bb', 'cards': []},
              'Player Aa has no cards',
              'Player Aa has: Three of Diamonds, Four of Hearts',
              [{'rank': 0, 'suit': 0}],
              [{'rank': 0, 'suit': 0}, {'rank': 1, 'suit': 1}, {'rank': 2, 'suit': 2}],
              0,
              2,
              ({'rank': 0, 'suit': 0}, []),
              ({'rank': 1, 'suit': 1}, [{'rank': 0, 'suit': 0}, {'rank': 2, 'suit': 2}])
             ]
    generic_value_tester(functions, values)

def card_matching_card1():
    c = per.Card(2, 12)
    return c.matching_card().__dict__

def card_matching_card2():
    c = per.Card(3, 5)
    return c.matching_card().__dict__

def unordered_print_tester(f, args):
    """Return the lines printed to std output by the function f on arguments
    args **as a Counter object**.
    """
    stream = io.StringIO()
    with redirect_stdout(stream):
        f(*args)
    return collections.Counter(stream.getvalue().split('\n')[:-1])

def player_remove_matches1():
    p = per.Player('A')
    p.cards = [per.Card(0, 0), per.Card(3, 0)]
    r = unordered_print_tester(p.remove_matches, [])
    return (r, [ c.__dict__ for c in p.cards ])

def player_remove_matches2():
    p = per.Player('A')
    p.cards = [per.Card(0, 0), per.Card(2, 0)]
    r = unordered_print_tester(p.remove_matches, [])
    return (r, [ c.__dict__ for c in p.cards ])

def player_remove_matches3():
    p = per.Player('A')
    p.cards = [per.Card(0, 0), per.Card(1, 1), per.Card(3, 0), per.Card(3, 1), per.Card(2, 1)]
    r = unordered_print_tester(p.remove_matches, [])
    return (r, [ c.__dict__ for c in p.cards ])

def test_remove_matches():
    functions = [card_matching_card1, card_matching_card2, player_remove_matches1,
                 player_remove_matches2, player_remove_matches3]
    values = [{'rank': 12, 'suit': 1},
              {'rank': 5, 'suit': 0},
              (collections.Counter({'Player A: Two of Clubs matches Two of Spades': 1}), []),
              (collections.Counter(), [{'rank': 0, 'suit': 0}, {'rank': 0, 'suit': 2}]),
              (collections.Counter({'Player A: Three of Diamonds matches Three of Hearts': 1, 'Player A: Two of Clubs matches Two of Spades': 1}), [{'rank': 1, 'suit': 3}])
             ]
    generic_value_tester(functions, values)
    
def cardgame_init1():
    g = per.CardGame(['A', 'B'],0)
    return ([ c.__dict__ for c in g.deck.cards ], g.numcards, [ p.__dict__ for p in g.players ])

def cardgame_init2():
    g = per.CardGame(['A', 'B', 'C', 'D', 'E'], 10)
    return ([ c.__dict__ for c in g.deck.cards ], g.numcards, [ p.__dict__ for p in g.players ])

def cardgame_str1():
    g = per.CardGame(['A', 'B'],0)
    return str(g)

def cardgame_str2():
    g = per.CardGame(['A', 'B', 'C'],0)
    g.players[0].cards = [per.Card(0, 0)]
    g.players[2].cards = [per.Card(1, 1), per.Card(2, 2)]
    return str(g)

def cardgame_shuffle1():
    g = per.CardGame(['A', 'B'],0)
    cs1 = g.deck.cards[:]
    g.shuffle_deck()
    return generic_permutation_tester(g.deck.cards, cs1)
    
def test_cardgame():
    functions = [cardgame_init1, cardgame_init2,
                 cardgame_str1, cardgame_str2,
                 cardgame_shuffle1]
    values = [([{'rank': 0, 'suit': 0},
                {'rank': 1, 'suit': 0},
                {'rank': 2, 'suit': 0},
                {'rank': 3, 'suit': 0},
                {'rank': 4, 'suit': 0},
                {'rank': 5, 'suit': 0},
                {'rank': 6, 'suit': 0},
                {'rank': 7, 'suit': 0},
                {'rank': 8, 'suit': 0},
                {'rank': 9, 'suit': 0},
                {'rank': 11, 'suit': 0},
                {'rank': 12, 'suit': 0},
                {'rank': 0, 'suit': 1},
                {'rank': 1, 'suit': 1},
                {'rank': 2, 'suit': 1},
                {'rank': 3, 'suit': 1},
                {'rank': 4, 'suit': 1},
                {'rank': 5, 'suit': 1},
                {'rank': 6, 'suit': 1},
                {'rank': 7, 'suit': 1},
                {'rank': 8, 'suit': 1},
                {'rank': 9, 'suit': 1},
                {'rank': 10, 'suit': 1},
                {'rank': 11, 'suit': 1},
                {'rank': 12, 'suit': 1},
                {'rank': 0, 'suit': 2},
                {'rank': 1, 'suit': 2},
                {'rank': 2, 'suit': 2},
                {'rank': 3, 'suit': 2},
                {'rank': 4, 'suit': 2},
                {'rank': 5, 'suit': 2},
                {'rank': 6, 'suit': 2},
                {'rank': 7, 'suit': 2},
                {'rank': 8, 'suit': 2},
                {'rank': 9, 'suit': 2},
                {'rank': 10, 'suit': 2},
                {'rank': 11, 'suit': 2},
                {'rank': 12, 'suit': 2},
                {'rank': 0, 'suit': 3},
                {'rank': 1, 'suit': 3},
                {'rank': 2, 'suit': 3},
                {'rank': 3, 'suit': 3},
                {'rank': 4, 'suit': 3},
                {'rank': 5, 'suit': 3},
                {'rank': 6, 'suit': 3},
                {'rank': 7, 'suit': 3},
                {'rank': 8, 'suit': 3},
                {'rank': 9, 'suit': 3},
                {'rank': 10, 'suit': 3},
                {'rank': 11, 'suit': 3},
                {'rank': 12, 'suit': 3}],
               51,
               [{'name': 'A', 'cards': []}, {'name': 'B', 'cards': []}]),
              ([{'rank': 11, 'suit': 0},
                {'rank': 12, 'suit': 0},
                {'rank': 10, 'suit': 1},
                {'rank': 11, 'suit': 1},
                {'rank': 12, 'suit': 1},
                {'rank': 10, 'suit': 2},
                {'rank': 11, 'suit': 2},
                {'rank': 12, 'suit': 2},
                {'rank': 10, 'suit': 3},
                {'rank': 11, 'suit': 3},
                {'rank': 12, 'suit': 3}],
               11,
               [{'name': 'A', 'cards': []},
                {'name': 'B', 'cards': []},
                {'name': 'C', 'cards': []},
                {'name': 'D', 'cards': []},
                {'name': 'E', 'cards': []}]),
              'Player A has no cards\nPlayer B has no cards',
              'Player A has: Two of Clubs\nPlayer B has no cards\nPlayer C has: Three of Diamonds, Four of Hearts',
              True
             ]
    generic_value_tester(functions, values)

def deal_cards1():
    g = per.CardGame(['A', 'B'], 0)
    g.deal_cards()
    return [ [ c.__dict__ for c in p.cards ] for p in g.players ]

def deal_cards2():
    g = per.CardGame(['A', 'B', 'C', 'D', 'E'], 10)
    g.deal_cards()
    return [ [ c.__dict__ for c in p.cards ] for p in g.players ]

def test_deal_cards():
    functions = [deal_cards1, deal_cards2]
    values = [
[[{'rank': 12, 'suit': 3},
  {'rank': 10, 'suit': 3},
  {'rank': 8, 'suit': 3},
  {'rank': 6, 'suit': 3},
  {'rank': 4, 'suit': 3},
  {'rank': 2, 'suit': 3},
  {'rank': 0, 'suit': 3},
  {'rank': 11, 'suit': 2},
  {'rank': 9, 'suit': 2},
  {'rank': 7, 'suit': 2},
  {'rank': 5, 'suit': 2},
  {'rank': 3, 'suit': 2},
  {'rank': 1, 'suit': 2},
  {'rank': 12, 'suit': 1},
  {'rank': 10, 'suit': 1},
  {'rank': 8, 'suit': 1},
  {'rank': 6, 'suit': 1},
  {'rank': 4, 'suit': 1},
  {'rank': 2, 'suit': 1},
  {'rank': 0, 'suit': 1},
  {'rank': 11, 'suit': 0},
  {'rank': 8, 'suit': 0},
  {'rank': 6, 'suit': 0},
  {'rank': 4, 'suit': 0},
  {'rank': 2, 'suit': 0},
  {'rank': 0, 'suit': 0}],
 [{'rank': 11, 'suit': 3},
  {'rank': 9, 'suit': 3},
  {'rank': 7, 'suit': 3},
  {'rank': 5, 'suit': 3},
  {'rank': 3, 'suit': 3},
  {'rank': 1, 'suit': 3},
  {'rank': 12, 'suit': 2},
  {'rank': 10, 'suit': 2},
  {'rank': 8, 'suit': 2},
  {'rank': 6, 'suit': 2},
  {'rank': 4, 'suit': 2},
  {'rank': 2, 'suit': 2},
  {'rank': 0, 'suit': 2},
  {'rank': 11, 'suit': 1},
  {'rank': 9, 'suit': 1},
  {'rank': 7, 'suit': 1},
  {'rank': 5, 'suit': 1},
  {'rank': 3, 'suit': 1},
  {'rank': 1, 'suit': 1},
  {'rank': 12, 'suit': 0},
  {'rank': 9, 'suit': 0},
  {'rank': 7, 'suit': 0},
  {'rank': 5, 'suit': 0},
  {'rank': 3, 'suit': 0},
  {'rank': 1, 'suit': 0}]],
[[{'rank': 12, 'suit': 3}, {'rank': 10, 'suit': 2}, {'rank': 11, 'suit': 0}],
 [{'rank': 11, 'suit': 3}, {'rank': 12, 'suit': 1}],
 [{'rank': 10, 'suit': 3}, {'rank': 11, 'suit': 1}],
 [{'rank': 12, 'suit': 2}, {'rank': 10, 'suit': 1}],
 [{'rank': 11, 'suit': 2}, {'rank': 12, 'suit': 0}]]
             ]
    generic_value_tester(functions, values)

def simple_play1():
    g = per.CardGame(['A', 'B'],0)
    g.deal_cards()
    return unordered_print_tester(g.simple_play, [])

def simple_play2():
    g = per.CardGame(['A', 'B', 'C'], 10)
    g.deal_cards()
    return unordered_print_tester(g.simple_play, [])

def simple_play3():
    g = per.CardGame(['A', 'B', 'C'], 8)
    g.deal_cards()
    return unordered_print_tester(g.simple_play, [])

def simple_play4():
    g = per.CardGame(['A', 'B'],0)
    g.players[0].cards = [per.Card(0, 0), per.Card(3, 0)]
    g.players[1].cards = [per.Card(1, 1)]
    return unordered_print_tester(g.simple_play, [])

def test_simple_play():
    functions = [simple_play1, simple_play2, simple_play3, simple_play4]
    values = [collections.Counter({'Player A: Four of Spades matches Four of Clubs': 1, 'The winners are A and B': 1, 'Player A: Eight of Spades matches Eight of Clubs': 1, 'Player A: Ten of Spades matches Ten of Clubs': 1, 'Player B: Seven of Spades matches Seven of Clubs': 1, 'Player B: Three of Spades matches Three of Clubs': 1, 'Player B: Jack of Spades matches Jack of Clubs': 1, 'Player B: Nine of Spades matches Nine of Clubs': 1, 'Player A: Six of Spades matches Six of Clubs': 1, 'Player A: Two of Spades matches Two of Clubs': 1, 'Player B: Five of Spades matches Five of Clubs': 1}),
              collections.Counter({'Player B: King of Spades matches King of Clubs': 1, 'Player A: Ace of Spades matches Ace of Clubs': 1, 'Player C: Queen of Hearts matches Queen of Diamonds': 1, 'Player A: Ace of Hearts matches Ace of Diamonds': 1, 'Player B: King of Hearts matches King of Diamonds': 1, 'The winners are A and B': 1}),
              collections.Counter({'Player B: King of Spades matches King of Clubs': 1, 'Player A: Ace of Spades matches Ace of Clubs': 1, 'The winners are A and B': 1}),
              collections.Counter({'Player A: Two of Clubs matches Two of Spades': 1, 'The winner is A': 1})
             ]
    generic_value_tester(functions, values)

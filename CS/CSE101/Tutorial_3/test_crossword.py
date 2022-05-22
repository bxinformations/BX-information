#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct  5 15:02:39 2018

@author: mengel
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 28 00:29:56 2017

@author: smith
"""

import crossword

def test_read_row():
    """ Test cross.position_list. """
    tests = [
            (('#.#',), ['#', ' ', '#']),
            (('C.T',), ['C', ' ', 'T']),
            (('cat',), ['C', 'A', 'T']),
            (('',), []),
            (('%#!',), [' ', '#', ' ']),
            ]
    generic_tester(crossword.read_row,tests)

def test_read_clue():
    """Test crossword_read_clue()."""
    tests = [
            (('(0,1) down: Of or pertaining to the voice (5)',), ((0,1),'down',5,'Of or pertaining to the voice')),
            (('(0,3)  down :   Nocturnal winged mammals   (40) ',), ((0,3),'down',40,'Nocturnal winged mammals')),
            (('(10,10) across: Colourful underwater structure formed by colonies of organisms (5)',), ((10,10),'across',5,'Colourful underwater structure formed by colonies of organisms'))
            ]
    generic_tester(crossword.read_clue,tests)

def test_split_type():
    """Test crossword_split_type()."""
    tests = [
            (('ROW ##.##',), ('ROW', '##.##')),
            (('CLUE (0,1) down: Of or pertaining to the voice (5)',), ('CLUE', '(0,1) down: Of or pertaining to the voice (5)')),
            (('   ROW   ##.##   ',), ('ROW', '##.##')),
            (('   ROW   ##.##   AND  ',), ('ROW', '##.##   AND'))
            ]
    generic_tester(crossword.split_type,tests)

def test_read_file():
    grid_5x5 = [
        ['#', ' ', '#', ' ', '#',],
        [' ', ' ', ' ', ' ', ' ',],
        ['#', ' ', '#', ' ', '#',],
        ['#', ' ', '#', ' ', ' ',],
        [' ', ' ', ' ', '#', ' ',],
        ]

    clues_5x5 = [
        ((0,1),'down',5,'A'),
        ((0,3),'down',4,'B'),
        ((1,0),'across',5,'C'),
        ((3,3),'across',2,'D'),
        ((3,4),'down',3,'E'),
        ((4,0),'across',3,'F'),
        ]
    tests = [
            (('tinypuzzle.txt',), ([[' ']], [((0, 0), 'down', 1, 'A')])),
            (('puzzle1.txt',), (grid_5x5, clues_5x5)),
            (('puzzle2.txt',), (grid_5x5, clues_5x5))
            ]
    generic_tester(crossword.read_file,tests)

def test_create_clue_string():
    tests = [
            ((((2, 3), 'across', 4, 'Black bird'),), '(2,3) across: Black bird (4)'),
            ((((0,0), 'down', 100, 'A'),), '(0,0) down: A (100)')
            ]
    generic_tester(crossword.create_clue_string,tests)

def stripped_puzzle_string(grid, clues):
    return crossword.create_puzzle_string(grid, clues).strip('\n')

def test_create_puzzle_string():
    grid_5x5 = [
        ['#', ' ', '#', ' ', '#',],
        [' ', ' ', ' ', ' ', ' ',],
        ['#', ' ', '#', ' ', '#',],
        ['#', ' ', '#', ' ', ' ',],
        [' ', ' ', ' ', '#', ' ',],
        ]

    clues_5x5 = [
        ((0,1),'down',5,'A'),
        ((0,3),'down',4,'B'),
        ((1,0),'across',5,'C'),
        ((3,3),'across',2,'D'),
        ((3,4),'down',3,'E'),
        ((4,0),'across',3,'F'),
        ]
    tests = [
            (([[' ']], [((0, 0), 'down', 1, 'A')]), '     0   \n  +-----+\n  |     |\n 0|     |\n  |     |\n  +-----+\n\n(0,0) down: A (1)'),
            ((grid_5x5, clues_5x5), '     0     1     2     3     4   \n  +-----+-----+-----+-----+-----+\n  |#####|     |#####|     |#####|\n 0|#####|     |#####|     |#####|\n  |#####|     |#####|     |#####|\n  +-----+-----+-----+-----+-----+\n  |     |     |     |     |     |\n 1|     |     |     |     |     |\n  |     |     |     |     |     |\n  +-----+-----+-----+-----+-----+\n  |#####|     |#####|     |#####|\n 2|#####|     |#####|     |#####|\n  |#####|     |#####|     |#####|\n  +-----+-----+-----+-----+-----+\n  |#####|     |#####|     |     |\n 3|#####|     |#####|     |     |\n  |#####|     |#####|     |     |\n  +-----+-----+-----+-----+-----+\n  |     |     |     |#####|     |\n 4|     |     |     |#####|     |\n  |     |     |     |#####|     |\n  +-----+-----+-----+-----+-----+\n\n(0,1) down: A (5)\n(0,3) down: B (4)\n(1,0) across: C (5)\n(3,3) across: D (2)\n(3,4) down: E (3)\n(4,0) across: F (3)')
            ]
    generic_tester(stripped_puzzle_string,tests)

def test_fill_in_word():
    grid_1 = [
        ['#', ' ', '#', ' ', '#',],
        [' ', ' ', ' ', ' ', ' ',],
        ['#', ' ', '#', ' ', '#',],
        ['#', ' ', '#', ' ', ' ',],
        [' ', ' ', ' ', '#', ' ',],
        ]
    grid_2 = [
        ['#', ' ', '#', ' ', '#',],
        [' ', ' ', ' ', ' ', ' ',],
        ['#', ' ', '#', ' ', '#',],
        ['#', ' ', '#', ' ', ' ',],
        ['C', 'A', 'T', '#', ' ',],
        ]
    grid_3 = [
        ['#', 'P', '#', ' ', '#',],
        [' ', 'I', ' ', ' ', ' ',],
        ['#', 'Z', '#', ' ', '#',],
        ['#', 'Z', '#', ' ', ' ',],
        ['C', 'A', 'T', '#', ' ',],
        ]
    grid_4 = [
        ['#', 'P', '#', ' ', '#',],
        ['T', 'I', 'G', 'E', 'R',],
        ['#', 'Z', '#', ' ', '#',],
        ['#', 'Z', '#', ' ', ' ',],
        ['C', 'A', 'T', '#', ' ',],
        ]
    grid_5 = [
        ['#', 'P', '#', ' ', '#',],
        ['T', 'I', 'G', 'E', 'R',],
        ['#', 'Z', '#', ' ', '#',],
        ['#', 'Z', '#', 'N', 'O',],
        ['C', 'A', 'T', '#', ' ',],
        ]
    grid_6 = [
        ['#', 'P', '#', ' ', '#',],
        ['T', 'I', 'G', 'E', 'R',],
        ['#', 'Z', '#', ' ', '#',],
        ['#', 'Z', '#', 'N', 'O',],
        ['C', 'A', 'T', '#', 'N',],
        ]
    tests = [
            ((grid_1, 'CAT', (4,0), 'across'), grid_2),
            ((grid_2, 'PIZZA', (0,1), 'down'), grid_3),
            ((grid_3, 'TIGER', (1,0), 'across'), grid_4),
            ((grid_4, 'NO', (3,3), 'across'), grid_5),
            ((grid_5, 'ON', (3,4), 'down'), grid_6),
            ]
    generic_tester(crossword.fill_in_word,tests)


def test_create_row_string():
    grid = [
        ['#', ' ', '#', ' ', '#',],
        [' ', ' ', ' ', ' ', ' ',],
        ['#', ' ', '#', ' ', '#',],
        ['#', ' ', '#', ' ', ' ',],
        [' ', ' ', ' ', '#', ' ',],
        ]
    tests = [
            ((grid[0],), '#.#.#'),
            ((grid[1],), '.....'),
            ((grid[2],), '#.#.#'),
            ((grid[3],), '#.#..'),
            ((grid[4],), '...#.')
            ]
    generic_tester(crossword.create_row_string,tests)

def test_write_puzzle():
    grid_5x5 = [
        ['#', ' ', '#', ' ', '#',],
        [' ', ' ', ' ', ' ', ' ',],
        ['#', ' ', '#', ' ', '#',],
        ['#', ' ', '#', ' ', ' ',],
        [' ', ' ', ' ', '#', ' ',],
        ]
    clues_5x5 = [
        ((0,1),'down',5,'A'),
        ((0,3),'down',4,'B'),
        ((1,0),'across',5,'C'),
        ((3,3),'across',2,'D'),
        ((3,4),'down',3,'E'),
        ((4,0),'across',3,'F'),
        ]
    args = ('test_puzzle.out', grid_5x5, clues_5x5)
    generic_output_file_tester(crossword.write_puzzle, args,
                               'test_puzzle.out','puzzle1.txt')



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

def generic_output_file_tester(f, arguments, outputfile, ref_outputfile):
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
            counter = 1
            with open(outputfile) as ofile, open(ref_outputfile) as rfile:
                for oline, refline in zip(ofile, rfile):
                    if oline != refline:
                        print(('\nFAILED (difference in line {}:\n'
                            + 'got {}: {}\n'
                            + 'but expected {} {}').format(
                                counter, outputfile, oline.strip(),
                                ref_outputfile, refline.strip()))
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


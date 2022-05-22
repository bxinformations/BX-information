#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 10 10:13:53 2019

@author: peixin.you
"""

def split_type(line):
    """Splits off the first word in the line and returns both parts in a tuple.
    Also eliminates all leading and trailing spaces.
    Example:
        split_type('ROW ##.##') returns ('ROW', '##.##')
        split_type('CLUE (0,1) down: Of or pertaining to the voice (5)') returns
            ('CLUE', '(0,1) down: Of or pertaining to the voice (5)')
        split_type('  ROW    ##.##   ') returns ('ROW', '##.##')

    """
    pos = line.find('ROW')
    if pos != -1:
        flag = 'ROW'
    else:
        flag = 'CLUE'
    line = line.strip()
    line = line.strip(flag)
    line = line.strip()
    return (flag, line)

def read_row(row):
    """Reads a row of a crossword puzzle and decomposes it into a list. Every 
    '#' is blocking the current box. Letters 'A', ..., 'Z' and 'a', ..., 'z'
    are values that are already filled into the box. These letters are capitalized
    and then put into the list. All other characters stand 
    for empty boxes which are represented by a space ' ' in the list.
    Examples:
        read_row('#.#') gives ['#', ' ', '#']
        read_row('C.T') gives ['C', ' ', 'T']
        read_row('cat') gives ['C', 'A', 'T']
    """
    ansRow = []
    for s in row:
        if s.isalpha() or s == '#':
            ansRow.append(s.upper())
        else:
            ansRow.append(' ')
    return ansRow

def read_clue(cluestring):
    """Reads a clue into a tuple in the following way: The input is of the form
        '(x,y) direction: question (length)
    where x, y and length are integers, direction is 'across' or 'down'
    and question is the text of the clue. The output should then be
        ((x, y), direction, length, question)
    where (x, y) is a tuple of values of type int and length is of type int.
    None of these values are strings. There may be arbitrarily many spaces 
    between the different parts of the input.
    Example:
        read_clue('(0,1) down: Of or pertaining to the voice (5)') returns
        ((0, 1), 'down', 5, 'Of or pertaining to the voice')
    """
    posL = cluestring.find('(')
    posR = cluestring.find(')')
    Pos = cluestring[posL:posR + 1]
    posM = Pos.find(',')
    posX = Pos[1:posM]
    posY = Pos[posM + 1:-1]
    cluestring = cluestring[posR + 2:]
    pos = cluestring.find('down')
    if pos != -1:
        flag = 'down'
    else:
        flag = 'across'
    cluestring = cluestring.strip()
    cluestring = cluestring.strip(flag)
    cluestring = cluestring.strip()
    cluestring = cluestring.strip(':')
    cluestring = cluestring.strip()
    posL = cluestring.find('(')
    posR = cluestring.find(')')
    num = cluestring[posL:posR + 1]
    cluestring = cluestring.strip()
    cluestring = cluestring.strip(num)
    cluestring = cluestring.strip()
    num = num.strip('(')
    num = num.strip(')')
    return ((int(posX), int(posY)), flag, int(num), cluestring)

def read_file(filename):
    """Opens the file with the given filename and creates the puzzle in it. 
    Returns a pair consisting of the puzzle grid and the list of clues. Assumes
    that the first line gives the size. Afterwards, the rows and clues are given.
    The description of the rows and clues may interleave arbitrarily.
    """
    fileIn = open(filename, "r")
    size = fileIn.readline()
    size = size[4:]
    size = int(size)
    ansR = [[] * (size) for i in range(size)]
    ansC = [[] * (size) for i in range(size)]
    totC = totR = 0
    for i in range(2):
        for j in range(size):
            line = fileIn.readline()
            flag, line = split_type(line)
            if flag == 'ROW':
                ansR[totR] = read_row(line)
                totR += 1
            else:
                ansC[totC] = read_clue(line)
                totC += 1
    return (ansR, ansC)

def create_clue_string(clue):
    """ Given a clue, which is a tuple
    (position,direction,length,question),
    creat a string in the form 'position direction: question (length)'.
    For example, given the clue
    ((2, 3),'across',4,'Black bird'),
    this function will return
    (2,3) across: Black bird (4)
    """
    pos, flag, num, word = clue
    posX, posY = pos
    return ('({},{}) {}: {} ({})'.format(posX,posY,flag,word,num))

def create_grid_string(grid):
    """Print out a crossword grid."""
    size = len(grid)
    col_no_line = '   '
    for j in range(size):
        col_no_line += ' {:2}   '.format(j)
    sep_line = '  +' + ('-----+')*size
    result = col_no_line + '\n' + sep_line + '\n'
    for (i, row) in enumerate(grid):
        above = '  |'
        centre = '{:2}|'.format(i)
        below = '  |'
        for c in row:
            if c == '#':
                above += '#####|'
                centre += '#####|'
                below += '#####|'
            else:
                above += '     |'
                centre += '  ' + c + '  |'
                below += '     |'
        result += above + '\n' + centre + '\n' + below + '\n'
        result += sep_line + '\n'
    return result

def create_puzzle_string(grid, clues):
    """Create a human readable string representation of the puzzle."""
    p = create_grid_string(grid)
    for i in range(len(clues)):
        p += create_clue_string(clues[i]) + '\n'
    return p

def fill_in_word(grid, word, position, direction):
    """Fill in the given word into the grid. The given direction is either 'down' 
    or 'across'. position gives the coordinates of the first letter of the word
    in the grid."""
    posX, posY = position
    print(len(grid[4]))
    if direction == 'across':
        for i in range(posY, len(word) + posY):
            grid[posX][i] = word[i - posY]
    else:
        for i in range(posX, len(word) + posX):
            grid[i][posY] = word[i - posX]
    return grid

def create_row_string(row):
    """Returns a row representation of a string.
    Example:
        create_row_string(['#', 'A', ' ']) returns '#A.'
    """
    ansRow = ''
    ansRow = ansRow.join(row)
    ansRow = ansRow.replace(' ', '.')
    return ansRow

def write_puzzle(filename, grid, clues):
    """Writes the puzzle given by the grid and by the clues to the specified
    file.
    """
    fileOut = open(filename, "w")
    fileOut.write('SIZE {}\n'.format(len(grid)))
    for i in range(len(grid)):
        fileOut.write('ROW ' + create_row_string(grid[i]) + '\n')
    for i in range(len(clues)):
        fileOut.write('CLUE ' + create_clue_string(clues[i]) + '\n')
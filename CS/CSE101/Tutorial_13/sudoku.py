# -*- coding: utf-8 -*-

class Sudoku:
    def __init__(self, filename):
        """Initilize rows, columns and squares as empty."""
        self.board = [([0]*9).copy() for _ in range(9)]
        self.read_board(filename)
        
    def read_board(self, filename):
        """Read a board from the given file."""
        with open(filename) as file:
            for (x, line) in enumerate(file):
                for (y, element) in enumerate(line):
                    if element in '123456789':
                        self.put(x, y, int(element))
                        
    def __str__(self):
        """Give a string representation of the board."""
        res = ''
        for x in range(9):
            if x == 0:
                res += '┌───┬───┬───┐\n'
            if x == 3 or x == 6:
                res += '├───┼───┼───┤\n'
            for y in range(9):
                if y % 3 == 0:
                    res += '│'
                if self.board[x][y] != 0:
                    res+=str(self.board[x][y])
                else:
                    res+='.'
            res+='│\n'
        res += '└───┴───┴───┘\n'
        return res

    def put(self, row, column, value):
        """Set value in cell (x,y)."""
        self.board[row][column] = value
        
    def delete(self, row, column):
        """Delete content of cell (x,y)."""
        self.board[row][column] = 0

    def check_row(self, row, value):
        """Return True if the value is already contained in the row."""
        return value in self.board[row]
    
    def check_column(self, column, value):
        """Return True if the value is already contained in the column."""
        for row in self.board:
            if value == row[column]:
                return True
        return False
    
    def check_box(self, row, column, value):
        """Return True if the 3x3 box that contains the cell with the given
        row and column contains already value.
        """
        for x in range(row//3*3, row//3*3 + 3): 
            for y in range(column//3*3, column//3*3 + 3):
                if value == self.board[x][y]:
                    return True
        return False
    
    def check(self, row, column, value):
        """Return True if the value is already in the row, column or box of
        the given cell."""
        return self.check_row(row, value) or \
            self.check_column(column, value) or \
            self.check_box(row, column, value)
        
    def find_first_empty(self):
        """Return the first empty cell if there is one, otherwise None."""
        for x in range(9):
            for y in range(9):
                if self.board[x][y] == 0:
                    return (x,y)
        return None

    def solve(self):
        """Solve the given Sudoku. When a solution is found, print 'Found solution'
        followed by the solution in the next line.
        """
        (x, y) = self.find_first_empty()
        for i in range(9):
            if self.check(x, y, i + 1) == False:
                self.put(x, y, i + 1)
                if self.find_first_empty() == None:
                    print('Found solution!')
                    print(self)
                    return True
                else:
                    if (self.solve()):
                        return True
                    self.delete(x, y)
                    

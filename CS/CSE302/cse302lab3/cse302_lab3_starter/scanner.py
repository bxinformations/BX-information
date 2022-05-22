import getopt
import sys
from py.ply import lex as lex


class Lexer(object):
    def __init__(self, filename="", **kwargs):
        self.filename = filename
        self.lexer = lex.lex(object=self, **kwargs)

    def input(self, text):
        self.lexer.input(text)

    def reset_lineno(self):
        self.lexer.lineno = 1

    def token(self):
        self.last_token = self.lexer.token()
        return self.last_token

    def find_tok_column(self, token):
        """ Find the column of the token in its line.
        """
        last_cr = self.lexer.lexdata.rfind('\n', 0, token.lexpos)
        return token.lexpos - last_cr

    # Test it output
    def test(self, data):
        self.input(data)
        while True:
            tok = self.token()
            if tok:
                print(tok)
            else:
                break

    ## PRIVATE ##

    keywords = {'print': 'PRINT', 'def': 'DEF',
                'var': 'VAR', 'bool': 'BOOL', 'int': 'INT', 'main': 'MAIN',
                'if': 'IF', 'else': 'ELSE',
                'while': 'WHILE',
                'break': 'BREAK', 'continue': 'CONTINUE',
                'true': 'TRUE', 'false': 'FALSE'}

    tokens = (
        'PLUS', 'MINUS', 'SEMICOLON', 'LPAREN', 'RPAREN', 'IDENT', 'NUMBER',
        'LBRACE', 'RBRACE',
        'BITSHL', 'BITSHR', 'BITXOR', 'BITOR', 'BITAND',
        'TIMES', 'DIV', 'MODULUS', 'UMINUS', 'BITCOMPL',
        'ASSIGN', 'COLON', 'OR', 'AND', 'EQUAL', 'DIFFERENT',
        'LESS', 'MORE', 'LESSEQ', 'MOREEQ', 'NOT',
    ) + tuple(keywords.values())

    t_DEF = r'def'
    t_LPAREN = r'\('
    t_RPAREN = r'\)'
    t_PLUS = r'\+'
    t_MINUS = '-'
    t_SEMICOLON = ';'
    t_BITSHR = r'\>\>'
    t_BITSHL = r'\<\<'
    t_BITXOR = r'\^'
    t_BITOR = r'\|'
    t_BITAND = r'\&'
    t_TIMES = r'\*'
    t_DIV = r'\/'
    t_MODULUS = r'\%'
    t_BITCOMPL = r'\~'
    t_ASSIGN = r'\='
    t_COLON = ':'
    t_LBRACE = r'\{'
    t_RBRACE = r'\}'
    t_OR = r'\|\|'
    t_AND = r'\&\&'
    t_EQUAL = r'\=\='
    t_DIFFERENT = r'\!\='
    t_LESS = r'\<'
    t_LESSEQ = r'\<\='
    t_MORE = r'\>'
    t_MOREEQ = r'\>\='
    t_NOT = r'\!'

    # Ignored characters
    t_ignore = " \t\f\v\r"

    def t_IDENT(self, t):
        r'[A-Za-z_][A-Za-z0-9_]*'
        t.type = self.keywords.get(t.value, 'IDENT')
        return t

    def t_NUMBER(self, t):
        r'0|[1-9][0-9]*'
        t.value = int(t.value)

        if t.value < 0 or t.value >= (1 << 63):
            print(
                t, f'{self.filename}:line {t.lexer.lineno}:column {self.find_tok_column(t.lexer)}:Error:Wrong integer "{t.value}" - out of accpeted range')
            raise SyntaxError('Number out of range [0, (1<<63)-1]')

        return t

    def t_COMMENTS(self, t):
        r'//.*\n?'
        t.lexer.lineno += 1

    def t_newline(self, t):
        r'\n'
        t.lexer.lineno += 1

    def t_error(self, t):
        print(
            t, f"{self.filename}:line {t.lexer.lineno}:column {self.find_tok_column(t.lexer)}:Error:Ilegal character '{t.value[0]}'")
        t.lexer.skip(1)
        raise SyntaxError(f'Unknown character')


if __name__ == "__main__":
    opts, args = getopt.getopt(sys.argv[1:], '', [])
    with open(args[0], 'r') as fp:
        data = fp.read()

    print(data)
    lexer = Lexer()
    lexer.test(data)

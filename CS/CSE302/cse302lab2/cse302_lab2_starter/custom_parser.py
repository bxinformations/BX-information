import getopt
import sys
from py.ply import yacc as yacc
from scanner import Lexer
import create_ast as ast


class Parser(object):
    def __init__(self):
        self.lex = Lexer()
        self.tokens = self.lex.tokens
        self.parser = yacc.yacc(module=self, start='program')

    def parse(self, text):
        return self.parser.parse(input=text, lexer=self.lex)

    precedence = (
        ('left', 'BITOR'),
        ('left', 'BITXOR'),
        ('left', 'BITAND'),
        ('left', 'BITSHL', 'BITSHR'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIV', 'MODULUS'),
        ('right', 'UMINUS'),
        ('right', 'BITCOMPL')
    )

    def p_error(self, p):
        if not p:
            raise SyntaxError(
                'Invalid program passed, check for the definition of main function')
        else:
            print(
                f'Unexpected/Wrong sign "{p.value}" at line {p.lineno} and at postion {p.lexpos}')
            raise SyntaxError(f'Unexpected/Wrong sign of type "{p.type}"')

    def p_expr_ident(self, p):
        """expr : IDENT"""
        p[0] = ast.Variable(p[1], p.lineno(1))

    def p_expr_number(self, p):
        """expr : NUMBER"""
        p[0] = ast.Number(p[1], p.lineno(1))

    def p_expr_binop(self, p):
        '''expr : expr PLUS expr
                | expr MINUS expr
                | expr TIMES expr
                | expr DIV expr
                | expr MODULUS expr
                | expr BITSHR expr
                | expr BITSHL expr
                | expr BITXOR expr
                | expr BITOR expr
                | expr BITAND expr'''

        to_name = {'+': 'PLUS', '-': 'MINUS', '*': 'TIMES',
                   '/': 'DIV', '%': 'MODULUS', '>>': 'BITSHR', '<<': 'BITSHL',
                   '^': 'BITXOR', '|': 'BITOR', '&': 'BITAND'}

        p[0] = ast.BinopApp(p[1], to_name[p[2]], p[3], p.lineno(2))

    def p_expr_unop(self, p):
        '''expr : BITCOMPL expr
                | MINUS expr %prec UMINUS
                | UMINUS expr'''

        to_name = {'-': 'UMINUS', '~': 'BITCOMPL'}
        p[0] = ast.UnopApp(to_name[p[1]], p[2], p.lineno(1))

    def p_expr_parens(self, p):
        '''expr : LPAREN expr RPAREN'''
        p[0] = p[2]

    def p_vardecl(self, p):
        '''stmt : VAR IDENT ASSIGN expr COLON INT SEMICOLON'''
        p[0] = ast.VarDec(p[2], p[4], p[6], p.lineno(2))

    def p_assign(self, p):
        '''stmt : IDENT ASSIGN expr SEMICOLON'''
        p[0] = ast.Assign(ast.Variable(p[1], p.lineno(1)),
                          p[3], lineno=p.lineno(3))

    def p_print(self, p):
        '''stmt : PRINT LPAREN expr RPAREN SEMICOLON'''
        p[0] = ast.Call(function="print",  args=[p[3]], lineno=p.lineno(1))

    def p_stmt(self, p):
        '''stmts : stmt stmts
                |'''
        if len(p) > 1:
            p[0] = [p[1]] + p[2]
        else:
            p[0] = []

    def p_function(self, p):
        '''body : LBRACE stmts RBRACE'''
        p[0] = p[2]

    def p_program(self, p):
        '''program : DEF MAIN LPAREN RPAREN body'''
        p[0] = p[5]


if __name__ == "__main__":
    opts, args = getopt.getopt(sys.argv[1:], '', [])
    with open(args[0], 'r') as fp:
        data = fp.read()

    print(data)
    lexer = Lexer()
    # lexer.test(data)
    parser = Parser()
    p = parser.parse(data)
    print(p)

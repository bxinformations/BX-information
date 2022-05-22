import getopt
import sys
from py.ply import yacc as yacc
from scanner import Lexer
import create_ast as ast


class Parser(object):
    def __init__(self, filename=""):
        self.lex = Lexer(filename=filename)
        self.tokens = self.lex.tokens
        self.parser = yacc.yacc(module=self, start='program')
        self.filename = filename

    def parse(self, text):
        return self.parser.parse(input=text, lexer=self.lex)

    precedence = (
        ('left', 'OR'),
        ('left', 'AND'),
        ('left', 'BITOR'),
        ('left', 'BITXOR'),
        ('left', 'BITAND'),
        ('nonassoc', 'EQUAL', 'DIFFERENT'),
        ('nonassoc', 'LESS', 'MORE', 'LESSEQ', 'MOREEQ'),
        ('left', 'BITSHL', 'BITSHR'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIV', 'MODULUS'),
        ('right', 'UMINUS', 'NOT'),
        ('right', 'BITCOMPL')
    )

    def p_error(self, p):
        if not p:
            raise SyntaxError(
                'Invalid program passed, check for the definition of main function')
        else:
            print(
                f'{self.filename}:line {p.lineno}:column {self.lex.find_tok_column(p)}:Error:Unexpected/Wrong sign "{p.value}"')
            raise SyntaxError(f'Unexpected/Wrong sign of type "{p.type}"')

    def p_expr_ident(self, p):
        """expr : IDENT"""
        p[0] = ast.Variable(p[1], p.lineno(1))

    def p_expr_number(self, p):
        """expr : NUMBER"""
        p[0] = ast.Number(p[1], p.lineno(1))

    def p_expr_bool(self, p):
        """expr : TRUE
                | FALSE"""
        p[0] = ast.Bool(p[1], p.lineno(1))

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
                | expr BITAND expr
                | expr OR expr
                | expr AND expr
                | expr EQUAL expr
                | expr DIFFERENT expr
                | expr LESS expr
                | expr MORE expr
                | expr LESSEQ expr
                | expr MOREEQ expr'''

        to_name = {'+': 'PLUS', '-': 'MINUS', '*': 'TIMES',
                   '/': 'DIV', '%': 'MODULUS', '>>': 'BITSHR', '<<': 'BITSHL',
                   '^': 'BITXOR', '|': 'BITOR', '&': 'BITAND', '||': 'OR',
                   '&&': 'AND', '==': 'EQUAL', '!=': 'DIFFERENT', '<': 'LESS',
                   '>': 'MORE', '<=': 'LESSEQ', '>=': 'MOREEQ'}

        p[0] = ast.BinopApp(p[1], to_name[p[2]], p[3], p.lineno(2))

    def p_expr_unop(self, p):
        '''expr : BITCOMPL expr
                | MINUS expr %prec UMINUS
                | UMINUS expr
                | NOT expr'''

        to_name = {'-': 'UMINUS', '~': 'BITCOMPL', '!': 'NOT'}
        p[0] = ast.UnopApp(to_name[p[1]], p[2], p.lineno(1))

    def p_expr_parens(self, p):
        '''expr : LPAREN expr RPAREN'''
        p[0] = p[2]

    def p_block(self, p):
        '''block : LBRACE stmts RBRACE'''
        p[0] = ast.Block(p[2])

    def p_decl_type(self, p):
        '''type : INT
                | BOOL'''
        p[0] = p[1].lower()

    def p_vardecl(self, p):
        '''stmt : VAR IDENT ASSIGN expr COLON type SEMICOLON'''
        p[0] = ast.VarDec(p[2], p[4], p[6], p.lineno(2))

    def p_assign(self, p):
        '''stmt : IDENT ASSIGN expr SEMICOLON'''
        p[0] = ast.Assign(ast.Variable(p[1], p.lineno(1)),
                          p[3], lineno=p.lineno(3))

    def p_continue(self, p):
        '''stmt : CONTINUE SEMICOLON'''
        p[0] = ast.StructuredJump(jump_type="continue", lineno=p.lineno(1))

    def p_break(self, p):
        '''stmt : BREAK SEMICOLON'''
        p[0] = ast.StructuredJump(jump_type="break", lineno=p.lineno(1))

    def p_print(self, p):
        '''stmt : PRINT LPAREN expr RPAREN SEMICOLON'''
        p[0] = ast.Call(function="print",  args=[p[3]], lineno=p.lineno(1))

    def p_ifelse(self, p):
        '''ifelse : IF LPAREN expr RPAREN block ifrest'''
        p[0] = ast.If(p[3], p[5], p[6], lineno=p.lineno(1))

    def p_ifrest(self, p):
        '''ifrest : ELSE ifelse
                | ELSE block
                |'''
        if len(p) == 1:
            p[0] = None
        else:
            p[0] = p[2]

    def p_while(self, p):
        '''while : WHILE LPAREN expr RPAREN block'''
        p[0] = ast.While(p[3], p[5], lineno=p.lineno(1))

    def p_stmt(self, p):
        '''stmts : stmt stmts
                | while stmts
                | ifelse stmts
                |'''
        p[0] = [p[1]] + p[2] if len(p) > 1 else []

    def p_program(self, p):
        '''program : DEF MAIN LPAREN RPAREN block'''
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

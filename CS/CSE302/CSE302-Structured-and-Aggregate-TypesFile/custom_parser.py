import getopt
import sys
from py.ply import yacc as yacc
from scanner import Lexer
import create_ast as ast


class Parser(object):
    def __init__(self, code, filename=""):
        self.lex = Lexer(filename=filename)
        self.tokens = self.lex.tokens
        self.parser = yacc.yacc(module=self, start='program')
        self.filename = filename
        self.code = code

    def parse(self):
        return self.parser.parse(input=self.code, lexer=self.lex)

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
        ('right', 'BITCOMPL'),
        ('right', 'REF', 'DEREF'),
        ('left', 'DOT', 'LBRACKET', 'RBRACKET', 'TO'),
    )

    def p_error(self, p):
        if not p:
            raise SyntaxError(
                'Invalid program passed, check for the definition of main function')
        else:
            col = self.lex.find_tok_column(p)
            print(
                f'{self.filename}:line {p.lineno}:column {col}:Error:Unexpected/Wrong sign "{p.value}"')
            line = self.code.split('\n')[p.lineno - 1]
            print(f'> {line}')
            print(' ' * (col + 1) + '^')
            raise SyntaxError(f'Unexpected/Wrong sign of type "{p.type}"')

    def p_expr_number(self, p):
        """expr : NUMBER"""
        p[0] = ast.Number(p[1], p.lineno(1), self.lex.find_tok_column(p, 1))

    def p_expr_bool(self, p):
        """expr : TRUE
                | FALSE"""
        p[0] = ast.Bool(p[1], p.lineno(1), self.lex.find_tok_column(p, 1))

    def p_expr_null(self, p):
        """expr : NULL"""
        p[0] = ast.Null(p.lineno(1), self.lex.find_tok_column(p, 1))

    def p_expr_assignable(self, p):
        '''expr : assignable'''
        p[0] = p[1]
    
    def p_expr_ref(self, p):
        '''expr : BITAND assignable'''
        p[0] = ast.Reference(p[2], p.lineno(
            1), self.lex.find_tok_column(p, 1))

    def p_expr_alloc(self, p):
        '''expr : ALLOC type LBRACKET expr RBRACKET
                | ALLOC type LBRACKET NUMBER RBRACKET'''
        arg = p[4]
        if isinstance(p[4], int):
            arg = ast.Number(p[4], None, None)

        p[0] = ast.Alloc(arg, p[2], p.lineno(
            1), self.lex.find_tok_column(p, 1))

    def p_expr_parens(self, p):
        '''expr : LPAREN expr RPAREN'''
        p[0] = p[2]

    def p_expr_seq(self, p):
        '''expr_seq : expr COMMA expr_seq
                    | expr'''
        p[0] = [p[1]] + (p[3] if len(p) > 2 else [])

    def p_exprs(self, p):
        '''exprs : expr COMMA expr_seq
                | expr
                |'''
        if len(p) == 1:
            p[0] = []
        elif len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[3]

    def p_expr_proc_call(self, p):
        '''expr : IDENT LPAREN exprs RPAREN'''
        p[0] = ast.Call(p[1], p[3], p.lineno(
            1), self.lex.find_tok_column(p, 1))

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

        p[0] = ast.BinopApp(p[1], to_name[p[2]], p[3], p.lineno(
            2), self.lex.find_tok_column(p, 2))

    def p_expr_unop(self, p):
        '''expr : BITCOMPL expr
                | MINUS expr %prec UMINUS
                | UMINUS expr
                | NOT expr'''

        to_name = {'-': 'UMINUS', '~': 'BITCOMPL', '!': 'NOT'}
        p[0] = ast.UnopApp(to_name[p[1]], p[2], p.lineno(1),
                           self.lex.find_tok_column(p, 1))

    def p_varinits(self, p):
        '''varinits : IDENT ASSIGN expr COMMA varinits
                    | IDENT ASSIGN expr'''
        p[0] = [(p[1], p[3], p.lineno(1), self.lex.find_tok_column(p, 1))
                ] + (p[5] if len(p) > 4 else [])

    def p_vardecl(self, p):
        '''vardecl : VAR varinits COLON type SEMICOLON'''
        p[0] = [ast.VarDec(var[0], var[1], p[4], var[2], var[3])
                for var in p[2]]

    def p_assignable(self, p):
        ''' assignable : IDENT 
                        | TIMES expr %prec DEREF
                        | expr LBRACKET expr RBRACKET
                        | expr DOT IDENT
                        | expr TO IDENT
                        '''
        if len(p) == 2:
            p[0] = ast.Variable(p[1], p.lineno(
                1), self.lex.find_tok_column(p, 1))
        elif p[1] == '*':
            p[0] = ast.DeRef(p[2], p.lineno(
                1), self.lex.find_tok_column(p, 1))
        elif p[2] == '[':
            p[0] = ast.Access(p[1], p[3], p.lineno(
                2), self.lex.find_tok_column(p, 2))
        elif p[2] == '.':
            p[0] = ast.Dot(p[1], ast.Variable(p[3], p.lineno(3),
                                              self.lex.find_tok_column(p, 3)), p.lineno(2), self.lex.find_tok_column(p, 2))
        elif p[2] == '->':
            p[0] = ast.To(p[1], ast.Variable(p[3], p.lineno(3),
                                             self.lex.find_tok_column(p, 3)), p.lineno(2), self.lex.find_tok_column(p, 2))

    def p_assign(self, p):
        '''assign : assignable ASSIGN expr SEMICOLON'''
        p[0] = ast.Assign(p[1], p[3], p.lineno(
            2), self.lex.find_tok_column(p, 2))

    def p_return(self, p):
        '''return : RETURN SEMICOLON
                | RETURN expr SEMICOLON'''
        p[0] = ast.Return(expr=p[2] if len(p) > 3 else None, lineno=p.lineno(
            1), col=self.lex.find_tok_column(p, 1))

    def p_jump(self, p):
        '''jump : CONTINUE SEMICOLON
                | BREAK SEMICOLON'''
        p[0] = ast.StructuredJump(jump_type=p[1], lineno=p.lineno(
            1), col=self.lex.find_tok_column(p, 1))

    def p_print(self, p):
        '''eval : expr SEMICOLON'''
        p[0] = ast.Eval(p[1], p.lineno(1), self.lex.find_tok_column(p, 1))

    def p_ifelse(self, p):
        '''ifelse : IF LPAREN expr RPAREN block ifrest'''
        p[0] = ast.If(p[3], p[5], p[6], lineno=p.lineno(1),
                      col=self.lex.find_tok_column(p, 1))

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
        p[0] = ast.While(p[3], p[5], lineno=p.lineno(
            1), col=self.lex.find_tok_column(p, 1))

    def p_stmt(self, p):
        '''stmts : vardecl stmts
                | block stmts
                | assign stmts
                | eval stmts
                | while stmts
                | ifelse stmts
                | jump stmts
                | return stmts
                |'''
        p[0] = [p[1]] + p[2] if len(p) > 1 else []

    def p_block(self, p):
        '''block : LBRACE stmts RBRACE'''
        p[0] = ast.Block(p[2], p.lineno(1), self.lex.find_tok_column(p, 1))

    def p_idents(self, p):
        '''idents : IDENT COMMA idents
                | IDENT'''
        p[0] = [(p[1], p.lineno(1), self.lex.find_tok_column(p, 1))] + \
            (p[3] if len(p) > 2 else [])

    def p_param(self, p):
        '''param : idents COLON type'''
        p[0] = [ast.Variable(var[0], var[1], var[2], p[3]) for var in p[1]]

    def p_params_arr(self, p):
        '''params_arr : param COMMA params_arr
                    | param'''
        p[0] = p[1] + (p[3] if len(p) > 2 else [])

    def p_params(self, p):
        '''params : param COMMA params_arr
                | param
                |'''
        if len(p) > 2:
            p[0] = p[1] + p[3]
        elif len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = []

    def p_proctype(self, p):
        '''proctype : COLON type
                    |'''
        p[0] = p[2] if len(p) > 1 else ast.VOID()

    def p_proc(self, p):
        '''procdecl : DEF IDENT LPAREN params RPAREN proctype block'''
        p[0] = ast.ProcDec(p[2], p[4], p[6], p[7], p.lineno(
            1), self.lex.find_tok_column(p, 1))

    def p_struct_field(self, p):
        ''' struct_field : IDENT COLON type '''
        p[0] = (ast.Variable(p[1], p.lineno(1),
                self.lex.find_tok_column(p, 1)), p[3])

    def p_struct_fields1(self, p):
        ''' struct_fields : struct_field
                            | struct_field COMMA struct_fields '''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[3]

    def p_type(self, p):
        '''type : INT
                | BOOL
                | IDENT
                | type TIMES
                | type LBRACKET NUMBER RBRACKET
                | STRUCT LBRACE struct_fields RBRACE'''

        if len(p) == 2 and p[1] == 'int':
            p[0] = ast.INT(p.lineno(1), self.lex.find_tok_column(p, 1))
        elif len(p) == 2 and p[1] == 'bool':
            p[0] = ast.BOOL(p.lineno(1), self.lex.find_tok_column(p, 1))
        elif len(p) == 2:
            p[0] = p[1]
        elif p[2] == '*':
            p[0] = ast.Pointer(p[1], p.lineno(
                2), self.lex.find_tok_column(p, 2))
        elif p[2] == '[':
            if p[3] <= 0:
                print(
                    f'{self.filename}:line {p.lineno(3)}:Error:Unexpected size of an array "{p[3]}"')
                print(
                    f'{self.filename}:line {p.lineno(3)}:Info:Postive number expected')
                col = self.lex.find_tok_column(p, 3)
                line = self.code.split('\n')[p.lineno(3) - 1]
                print(f'> {line}')
                print(' ' * (col + 1) + '^')
                raise ValueError('Invalid array size')

            p[0] = ast.Array(ast.Number(p[3], p.lineno(3), self.lex.find_tok_column(p, 3)), p[1], p.lineno(
                2), self.lex.find_tok_column(p, 2))
        elif p[1] == 'struct':
            p[0] = ast.Struct(p[3], p.lineno(
                1), self.lex.find_tok_column(p, 1))


    def p_typedecl(self, p):
        ''' typedecl : TYPE IDENT ASSIGN type SEMICOLON'''
        p[0] = ast.TypeDecl(p[2], p[4], p.lineno(
            1), self.lex.find_tok_column(p, 1))

    def p_decl(self, p):
        '''decl : vardecl
                | procdecl
                | typedecl'''
        p[0] = p[1]

    def p_decls(self, p):
        '''decls : decl decls
                |'''
        p[0] = [p[1]] + p[2] if len(p) > 1 else []

    def p_program(self, p):
        '''program : decls'''
        p[0] = p[1]


if __name__ == "__main__":
    opts, args = getopt.getopt(sys.argv[1:], '', [])
    with open(args[0], 'r') as fp:
        data = fp.read()

    print(data)
    lexer = Lexer()
    # lexer.test(data)
    parser = Parser(data)
    p = parser.parse()
    for i in p:
        print(i)

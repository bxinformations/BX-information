scopes = [dict()]
filename = ''
lines = []


class ProcDec:
    def __init__(self, name, args, type, body, lineno, col):
        if name.startswith('__bx_'):
            print(
                f'{filename}:line {lineno}:Error:No user-defined procedure name can begin with "__bx_"')
            print(f'> {lines[lineno - 1]}')
            print(' ' * (col + 1) + '^')
            raise SyntaxError('Invalid name')

        self.name = name
        self.args = args
        self.type = type
        self.body = body
        self.lineno = lineno
        self.col = col

    def type_check(self):
        scopes.append(self.type)
        scopes.append(dict())
        for arg in self.args:
            if arg.name in scopes[-1]:
                print(
                    f'{filename}:line {arg.lineno}:Error:Argument "{arg.name}" already given within the procdure {self.name}')
                print(f'> {lines[arg.lineno - 1]}')
                print(' ' * (arg.col + 1) + '^')
                raise SyntaxError('Argument redeclaration')
            else:
                scopes[-1][arg.name] = (arg.type, arg.lineno)

        has_return = self.body.type_check()
        scopes.pop()
        scopes.pop()

        if self.type != 'void' and not has_return:
            print(f'{filename}:line {self.lineno}:Error:Function {self.name} does not return value on every possible code path')
            print(f'> {lines[self.lineno - 1]}')
            print(' ' * (self.col + 1) + '^')
            raise SyntaxError('Return missing')

    def to_tac(self):
        result = {"proc": self.name, "args": list(self.args), "body": []}
        for i in self.body:
            result["body"].append(i.to_tac())
        return result


class Statment:
    def __init__(self):
        return


class Block(Statment):
    def __init__(self, body, lineno, col):
        self.body = body
        self.lineno = lineno
        self.col = col

    def type_check(self):
        scopes.append(dict())
        has_return = False

        for stmt in self.body:
            if not isinstance(stmt, Statment):
                for s in stmt:
                    has_return = max(s.type_check(), has_return)
            else:
                has_return = max(stmt.type_check(), has_return)

        scopes.pop()

        return has_return


class VarDec(Statment):
    def __init__(self, name, initial, type, lineno, col):
        self.name = name
        self.initial = initial
        self.type = type
        self.lineno = lineno
        self.col = col

    def type_check(self):
        if self.name in scopes[-1]:
            print(
                f'{filename}:line {self.lineno}:Error:Redecalred variable "{self.name}" within the scope')
            print(
                f'{filename}:line {scopes[-1][self.name][1]}:Info:Declartion of variable "{self.name}"')
            print(f'> {lines[self.lineno - 1]}')
            print(' ' * (self.col + 1) + '^')
            raise SyntaxError('Variable redeclaration')

        self.initial.type_check()

        if self.type == 'void':
            print(
                f'{filename}:line {self.lineno}:Error:Variable "{self.name}" cannot be declared as VOID')
            print(f'> {lines[self.lineno - 1]}')
            print(' ' * (self.col + 1) + '^')
            raise TypeError('Unexpected declaration type')

        elif self.initial.type == 'void':
            print(
                f'{filename}:line {self.initial.lineno}:Error:Variable "{self.name}" cannot be initialized with expression of type VOID')
            print(f'> {lines[self.initial.lineno - 1]}')
            print(' ' * (self.initial.col + 1) + '^')
            raise TypeError('Unexpected expression type')

        if self.initial.type != self.type:
            print(
                f'{filename}:line {self.initial.lineno}:Error:Variable "{self.name}" of type "{self.type}" initialized with expression of different type "{self.initial.type}"')
            print(f'> {lines[self.initial.lineno - 1]}')
            print(' ' * (self.initial.col + 1) + '^')
            raise TypeError('Unexpected expression type')

        scopes[-1][self.name] = (self.initial.type, self.lineno)

        return False

    def to_tac(self):
        return {"var": self.name, "init": self.initial}


class Assign(Statment):
    def __init__(self, target, expr, lineno, col):
        self.target = target
        self.expr = expr
        self.lineno = lineno
        self.col = col

    def type_check(self):
        self.target.type_check()
        self.expr.type_check()
        self.type = self.target.type

        if self.expr.type == 'void':
            print(
                f'{filename}:line {self.expr.lineno}:Error:Variable "{self.target.name}" cannot be assigned with expression of type VOID')
            print(f'> {lines[self.expr.lineno - 1]}')
            print(' ' * (self.expr.col + 1) + '^')
            raise TypeError('Unexpected declaration type')

        if self.type != self.expr.type:
            print(
                f'{filename}:line {self.expr.lineno}:Error:Variable "{self.target.name}" of type "{self.type}" assigned with expression of different type "{self.expr.type}"')
            print(f'> {lines[self.expr.lineno - 1]}')
            print(' ' * (self.expr.col + 1) + '^')
            raise TypeError('Unexpected expression type')

        return False


class While(Statment):
    def __init__(self, condition, instructions, lineno, col):
        self.condition = condition
        self.instructions = instructions
        self.lineno = lineno
        self.col = col

    def type_check(self):
        self.condition.type_check()
        if self.condition.type != 'bool':
            print(f'{filename}:line {self.condition.lineno}:Error:Condition in WHILE has to be of "bool" type, "{self.condition.type}" given')
            print(f'> {lines[self.condition.lineno - 1]}')
            print(' ' * (self.condition.col + 1) + '^')
            raise TypeError('Invalid condition type')

        self.instructions.type_check()
        return False


class If(Statment):
    def __init__(self, condition, instructions, else_case, lineno, col):
        self.condition = condition
        self.instructions = instructions
        self.else_case = else_case
        self.lineno = lineno
        self.col = col

    def type_check(self):
        self.condition.type_check()
        if self.condition.type != 'bool':
            print(f'{filename}:line {self.condition.lineno}:Error:Condition in IF has to be of "bool" type, "{self.condition.type}" given')
            print(f'> {lines[self.condition.lineno - 1]}')
            print(' ' * (self.condition.col + 1) + '^')
            raise TypeError('Invalid condition type')

        has_return_if = self.instructions.type_check()
        has_return_else = False
        if self.else_case is not None:
            has_return_else = self.else_case.type_check()

        return has_return_if and has_return_else


class Eval(Statment):
    def __init__(self, expr, lineno, col):
        self.expr = expr
        self.lineno = lineno
        self.col = col

    def type_check(self):
        self.expr.type_check()
        self.type = self.expr.type
        return False


class StructuredJump(Statment):
    def __init__(self, jump_type, lineno, col):
        self.jump_type = jump_type
        self.lineno = lineno
        self.col = col

    def type_check(self):
        return False


class Return(Statment):
    def __init__(self, expr, lineno, col):
        self.expr = expr
        self.lineno = lineno
        self.col = col

    def type_check(self):
        if self.expr is not None:
            self.expr.type_check()
            self.type = self.expr.type

            if self.expr.type != scopes[1]:
                print(
                    f'{filename}:line {self.expr.lineno}:Error:Cannot return expression of type "{self.expr.type}"')
                print(
                    f'{filename}:line {self.lineno}:Info:Expected return type "{scopes[1]}"')
                print(f'> {lines[self.expr.lineno - 1]}')
                print(' ' * (self.expr.col + 1) + '^')
                raise TypeError('Invalid return type')
        else:
            self.type = "void"
            if scopes[1] != 'void':
                print(
                    f'{filename}:line {self.expr.lineno}:Error:Cannot return expression void type when function requires "{scopes[1]}"')
                print(f'> {lines[self.expr.lineno - 1]}')
                print(' ' * (self.expr.col + 1) + '^')
                raise TypeError('Invalid return type')

        return True


class Expr:
    def __init__(self):
        return


class Variable(Expr):
    def __init__(self, name, lineno, col, type=None):
        self.name = name
        self.lineno = lineno
        self.type = type
        self.col = col

    def type_check(self):
        for scope in reversed(scopes):
            if self.name in scope:
                if self.type is None:
                    self.type = scope[self.name][0]

                elif self.type != scopes[self.name][0]:
                    print(
                        f'{filename}:line {self.lineno}:Error:Variable "{self.name}" of type {self.type} has been declared with type "{scopes[self.name][0]}"')
                    print(
                        f'{filename}:line {scopes[self.name][1]}:Info:Variable "{self.name}" declaration')
                    print(f'> {lines[self.lineno - 1]}')
                    print(' ' * (self.col + 1) + '^')
                    raise TypeError('Invalid type')

                return
        else:
            print(
                f'{filename}:line {self.lineno}:Error:Undeclared variable "{self.name}"')
            print(f'> {lines[self.lineno - 1]}')
            print(' ' * (self.col + 1) + '^')
            raise SyntaxError('Undeclared variable')


class Number(Expr):
    def __init__(self, value, lineno, col):
        self.value = value
        self.lineno = lineno
        self.type = 'int'
        self.col = col

    def type_check(self):
        return


class Bool(Expr):
    def __init__(self, value, lineno, col):
        self.value = value
        self.lineno = lineno
        self.type = 'bool'
        self.col = col

    def type_check(self):
        return


class UnopApp(Expr):
    def __init__(self, op, arg, lineno, col):
        self.op = op
        self.arg = arg
        self.lineno = lineno
        self.col = col

    def type_check(self):
        self.arg.type_check()
        if self.op in ['BITCOMPL', 'MINUS', 'UMINUS']:
            if self.arg.type == 'int':
                self.type = 'int'
            else:
                print(
                    f"{filename}:line {self.arg.lineno}:Error:Operation and argumet's type '{self.arg.type}' not compatible")
                print(
                    f"{filename}:line {self.arg.lineno}:Info:Operation '{self.op}' requires argument of type INT")
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise TypeError('Invalid type')

        elif self.op == 'NOT':
            if self.arg.type == 'bool':
                self.type = 'bool'
            else:
                print(
                    f"{filename}:line {self.arg.lineno}:Error:Operation and argumet's type '{self.arg.type}' not compatible")
                print(
                    f"{filename}:line {self.arg.lineno}:Info:Operation '{self.op}' requires argument of type BOOL")
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise TypeError('Invalid type')
        else:
            print(
                f"{filename}:line {self.lineno}:Error:Unknown operation '{self.op}'")
            print(f'> {lines[self.lineno - 1]}')
            print(' ' * (self.col + 1) + '^')
            raise SyntaxError('Unknown operation')


class BinopApp(Expr):
    def __init__(self, arg_left, op, arg_right, lineno, col):
        self.arg_left = arg_left
        self.arg_right = arg_right
        self.op = op
        self.lineno = lineno
        self.col = col

    def type_check(self):
        self.arg_left.type_check()
        self.arg_right.type_check()
        if self.op in ['PLUS', 'MINUS', 'TIMES', 'DIV', 'MODULUS', 'BITSHR',
                       'BITSHL', 'BITXOR', 'BITOR', 'BITAND']:
            if self.arg_left.type == 'int' and self.arg_right.type == 'int':
                self.type = 'int'
            else:
                print(
                    f"{filename}:line {self.arg_right.lineno}:Error:Operation and argumets' types not compatible")
                print(
                    f"{filename}:line {self.arg_right.lineno}:Info:For given opration '{self.op}' both arguments shall be INT'")
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise TypeError('Invalid arguments type')

        elif self.op in ['AND', 'OR']:
            if self.arg_left.type == 'bool' and self.arg_right.type == 'bool':
                self.type = 'bool'
            else:
                print(
                    f"{filename}:line {self.arg_right.lineno}:Error:Operation and argumets' types not compatible")
                print(
                    f"{filename}:line {self.arg_right.lineno}:Info:For given opration '{self.op}' both arguments shall be BOOL'")
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise TypeError('Invalid arguments type')
        
        elif self.op in ['EQUAL', 'DIFFERENT']:
            if (self.arg_left.type == 'int' and self.arg_right.type == 'int') or (self.arg_left.type == 'bool' and self.arg_right.type == 'bool'):
                self.type = 'bool'
            else:
                print(
                    f"{filename}:line {self.arg_right.lineno}:Error:Operation and argumets' types not compatible")
                print(
                    f"{filename}:line {self.arg_right.lineno}:Info:For given opration '{self.op}' arguments shall be BOOL or INT'")
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise TypeError('Invalid arguments type')

        elif self.op in ['LESS', 'MORE', 'LESSEQ', 'MOREEQ']:
            if self.arg_left.type == 'int' and self.arg_right.type == 'int':
                self.type = 'bool'
            else:
                print(
                    f"{filename}:line {self.arg_right.lineno}:Error:Operation and argumets' types not compatible")
                print(
                    f"{filename}:line {self.arg_right.lineno}:Info:For given opration '{self.op}' arguments shall be INT'")
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise TypeError('Invalid arguments type')
        else:
            print(
                f"{filename}:line {self.lineno}:Error:Unknown operation '{self.op}'")
            print(f'> {lines[self.lineno - 1]}')
            print(' ' * (self.col + 1) + '^')
            raise SyntaxError('Unknown operation')


class Call(Expr):
    def __init__(self, function, args, lineno, col):
        self.function = function
        self.args = args
        self.lineno = lineno
        self.col = col

    def type_check(self):
        if self.function == 'print':
            self.type = "void"
            if len(self.args) != 1:
                print(
                    f'{filename}:line {self.lineno}:Error:Function "print" can only have one argument, {len(self.args)} given')
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise SyntaxError('Invalid argments')

            self.args[0].type_check()
            if self.args[0].type == "int":
                self.function = '__bx_print_int'
            elif self.args[0].type == "bool":
                self.function = '__bx_print_bool'
            else:
                print(
                    f'{filename}:line {self.args[0].lineno}:Error:Cannot print() expression of type "{self.args[0].type}"')
                print(f'> {lines[self.args[0].lineno - 1]}')
                print(' ' * (self.args[0].col + 1) + '^')
                raise TypeError('Invalid argument type')

        elif self.function in scopes[0]:
            self.type, lineno, args_types = scopes[0][self.function]

            if len(self.args) != len(args_types):
                print(f'{filename}:line {self.lineno}:Error:Function "{self.function}" has requires {len(args_types)} arguments, {len(self.args)} given')
                print(
                    f'{filename}:line {lineno}:Info:Definition of function "{self.function}"')
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise SyntaxError('Invalid argments')

            for i in range(len(self.args)):
                self.args[i].type_check()
                if self.args[i].type != args_types[i]:
                    print(
                        f'{filename}:line {self.args[i].lineno}:Error:Argument of number {i + 1} is of wrong type "{self.args[i].type}"')
                    print(
                        f'{filename}:line {lineno}:Info:Expexted type of {i + 1}th argument is "{args_types[i]}"')
                    print(f'> {lines[self.args[i].lineno - 1]}')
                    print(' ' * (self.args[i].col + 1) + '^')
                    raise TypeError('Invalid argument type')
        else:
            print(
                f'{filename}:line {self.lineno}:Error:Undeclared procedure "{self.function}"')
            print(f'> {lines[self.lineno - 1]}')
            print(' ' * (self.col + 1) + '^')
            raise SyntaxError('Undeclared procedure')

        return

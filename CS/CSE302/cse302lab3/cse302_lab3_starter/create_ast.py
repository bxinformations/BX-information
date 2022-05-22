scopes = []


class Statment:
    def __init__(self):
        return


class Block(Statment):
    def __init__(self, body):
        self.body = body

    def type_check(self):
        scopes.append(dict())
        for stmt in self.body:
            stmt.type_check()
        scopes.pop()


class VarDec(Statment):
    def __init__(self, name, initial, type, lineno):
        self.name = name
        self.initial = initial
        self.type = type
        self.lineno = lineno

    def type_check(self):
        if self.name in scopes[-1]:
            print(
                f':line {self.lineno}:Error:Redecalred variable "{self.name}" within the scope')
            print(
                f':line {scopes[-1][self.name][1]}:Info:Declartion of variable "{self.name}"')
            raise SyntaxError('Variable redeclaration')

        self.initial.type_check()
        if self.initial.type != self.type:
            print(
                f':line {self.initial.lineno}:Error:Variable "{self.name}" of type "{self.type}" initialized with expression of different type "{self.initial.type}"')
            raise SyntaxError('Wrong type')

        scopes[-1][self.name] = (self.initial.type, self.lineno)


class Assign(Statment):
    def __init__(self, target, expr, lineno):
        self.target = target
        self.expr = expr
        self.lineno = lineno

    def type_check(self):
        self.target.type_check()
        self.expr.type_check()
        self.type = self.target.type


class Call(Statment):
    def __init__(self, function, args, lineno):
        # to be imporoved, here is enough when print is the only accepted function (except main)
        self.function = function
        self.args = args
        self.lineno = lineno

    def type_check(self):
        # print should have void type?
        self.args[0].type_check()


class While(Statment):
    def __init__(self, condition, instructions, lineno):
        self.condition = condition
        self.instructions = instructions
        self.lineno = lineno

    def type_check(self):
        self.condition.type_check()
        self.instructions.type_check()


class If(Statment):
    def __init__(self, condition, instructions, else_case, lineno):
        self.condition = condition
        self.instructions = instructions
        self.else_case = else_case
        self.lineno = lineno

    def type_check(self):
        self.condition.type_check()
        self.instructions.type_check()
        if self.else_case is not None:
            self.else_case.type_check()


class StructuredJump(Statment):
    def __init__(self, jump_type, lineno):
        self.jump_type = jump_type
        self.lineno = lineno

    def type_check(self):
        return


class Expr:
    def __init__(self):
        return


class Variable(Expr):
    def __init__(self, name, lineno):
        self.name = name
        self.lineno = lineno

    def type_check(self):
        for scope in reversed(scopes):
            if self.name in scope:
                self.type = scope[self.name][0]
                return
        else:
            print(
                f':line {self.lineno}:Error:Undeclared variable "{self.name}"')
            raise SyntaxError('Undeclared variable')


class Number(Expr):
    def __init__(self, value, lineno):
        self.value = value
        self.lineno = lineno
        self.type = 'int'

    def type_check(self):
        return


class Bool(Expr):
    def __init__(self, value, lineno):
        self.value = value
        self.lineno = lineno
        self.type = 'bool'

    def type_check(self):
        return


class UnopApp(Expr):
    def __init__(self, op, arg, lineno):
        self.op = op
        self.arg = arg
        self.lineno = lineno

    def type_check(self):
        self.arg.type_check()
        if self.op in ['BITCOMPL', 'MINUS', 'UMINUS'] and self.arg.type == 'int':
            self.type = 'int'
        elif self.op == 'NOT':
            self.type = 'bool'
        else:
            print(
                f":line {self.arg.lineno}:Error:Operation and argumet's type '{self.arg.type}' not comatible")
            print(
                f":line {self.arg.lineno}:Info:Change operation or argument type")
            raise SyntaxError('Wrong type')


class BinopApp(Expr):
    def __init__(self, arg_left, op, arg_right, lineno):
        self.arg_left = arg_left
        self.arg_right = arg_right
        self.op = op
        self.lineno = lineno

    def type_check(self):
        self.arg_left.type_check()
        self.arg_right.type_check()
        if self.op in ['PLUS', 'MINUS', 'TIMES', 'DIV', 'MODULUS', 'BITSHR',
                       'BITSHL', 'BITXOR', 'BITOR', 'BITAND']:
            if self.arg_left.type == 'int' and self.arg_right.type == 'int':
                self.type = 'int'
            else:
                print(
                    f":line {self.arg_right.lineno}:Error:Operation and argumets' types not comatible")
                print(
                    f":line {self.arg_right.lineno}:Info:For given opration '{self.op}' both arguments shall be INT'")
                raise SyntaxError('Wrong type')

        elif self.op in ['OR', 'AND', 'EQUAL', 'DIFFERENT', 'LESS', 'MORE',
                         'LESSEQ', 'MOREEQ']:
            # maybe and or can't work with ints
            self.type = 'bool'
        else:
            print(
                f":line {self.arg_right.lineno}:Error:Unknown operation '{self.op}'")
            raise SyntaxError('Unknown operation')

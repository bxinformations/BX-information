scopes = [dict()]
registered_types = set()
filename = ''
lines = []


class Type:
    def __init__(self):
        return


class INT(Type):
    def __init__(self, lineno=None, col=None):
        self.lineno = lineno
        self.col = col

    def __str__(self):
        return 'int'

    def sizeof(self):
        return 8

    def __eq__(self, other):
        if isinstance(other, str) and other in registered_types:
            other = scopes[0][other][0]
            while isinstance(other, str):
                other = scopes[0][other][0]

        return isinstance(other, INT)

    def type_check(self):
        return


class BOOL(Type):
    def __init__(self, lineno=None, col=None):
        self.lineno = lineno
        self.col = col

    def __str__(self):
        return 'bool'

    def sizeof(self):
        return 8

    def __eq__(self, other):
        if isinstance(other, str) and other in registered_types:
            other = scopes[0][other][0]
            while isinstance(other, str):
                other = scopes[0][other][0]

        return isinstance(other, BOOL)

    def type_check(self):
        return


class VOID(Type):
    def __init__(self):
        return

    def __str__(self):
        return 'void'

    def sizeof(self):
        return 1

    def __eq__(self, other):
        if isinstance(other, str) and other in registered_types:
            other = scopes[0][other][0]
            while isinstance(other, str):
                other = scopes[0][other][0]

        return isinstance(other, VOID)

    def type_check(self):
        return


class Pointer(Type):
    def __init__(self, type, lineno=None, col=None):
        self.type = type
        self.lineno = lineno
        self.col = col

    def __str__(self):
        return '*' + str(self.type)

    def sizeof(self):
        return 8

    def __eq__(self, other):
        if isinstance(other, str) and other in registered_types:
            other = scopes[0][other][0]
            while isinstance(other, str):
                other = scopes[0][other][0]

        if isinstance(self.type, str):
            my_ty = scopes[0][self.type][0]
            while isinstance(my_ty, str):
                my_ty = scopes[0][my_ty][0]
        else:
            my_ty = self.type

        if not isinstance(other, Pointer):
            return False

        if isinstance(my_ty, VOID) or isinstance(other.type, VOID):
            return True

        return self.type == other.type

    def type_check(self):
        if isinstance(self.type, Type):
            self.type.type_check()
        elif self.type not in scopes[0]:
            raise SyntaxError(self.type)

class Array(Type):
    def __init__(self, length, type, lineno=None, col=None):
        self.length = length
        self.type = type
        self.lineno = lineno
        self.col = col

    def __str__(self):
        return str(self.type) + '[' + str(self.length) + ']'

    def sizeof(self):
        if isinstance(self.type, str):
            ty = scopes[0][self.type][0]
        else:
            ty = self.type

        return self.length.value * ty.sizeof()

    def __eq__(self, other):
        if isinstance(other, str) and other in registered_types:
            other = scopes[0][other][0]
            while isinstance(other, str):
                other = scopes[0][other][0]

        return isinstance(other, Array) and (self.length.value == other.length.value) and (self.type == other.type)

    def type_check(self):
        if isinstance(self.type, Type):
            self.type.type_check()
        elif self.type not in scopes[0]:
            raise SyntaxError(self.type)
        else:
            scopes[0][self.type][0].type_check()


class Struct(Type):
    def __init__(self, fields, lineno=None, col=None):
        self.fields = fields
        self.lineno = lineno
        self.col = col

    def __str__(self):
        return 'struct'

    def sizeof(self):
        size = 0
        for field in self.fields:
            size += field[1].sizeof()

        return size

    def __eq__(self, other):
        if isinstance(other, str) and other in registered_types:
            other = scopes[0][other][0]

        if not isinstance(other, Struct):
            return False

        if len(other.fields) != len(self.fields):
            return False

        for other_field, field in zip(other.fields, self.fields):            
            if other_field[0].name != field[0].name or other_field[1] != field[1]:
                return False

        return True

    def type_check(self):
        for field in self.fields:
            if isinstance(field[1], Type):
                field[1].type_check()
            elif field[1] not in scopes[0]:
                raise SyntaxError(field[1])
            else:
                scopes[0][field[1]][0].type_check()


class TypeDecl:
    def __init__(self, name, type, lineno, col):
        self.name = name
        self.type = type
        self.lineno = lineno
        self.col = col

    def type_check(self):
        try:
            if isinstance(self.type, Type):
                self.type.type_check()
            elif self.type not in scopes[0]:
                raise SyntaxError(self.type)
            else:
                ty = scopes[0][self.type][0]
                while isinstance(ty, str):
                    ty = scopes[0][ty][0]

                ty.type_check()

        except SyntaxError as err:
            print(
                f'{filename}:line {self.lineno}:Error:TypeDelartion declared with undeclared type "{err}"')
            print(f'> {lines[self.lineno - 1]}')
            print(' ' * (self.col + 1) + '^')
            raise SyntaxError('Invalid type')
        except RecursionError:
            print(
                f'{filename}:line {self.lineno}:Error:Declared type has infinity size')
            print(f'> {lines[self.lineno - 1]}')
            print(' ' * (self.col + 1) + '^')
            raise SyntaxError('Ill-formed type')


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

    def __str__(self):
        res = ''
        for i in self.body.body:
            if isinstance(i, list):
                for j in i:
                    res += '\n' + str(j)
            else:
                res += '\n' + str(i)
        return res

    def type_check(self):
        if isinstance(self.type, str):
            if self.type not in scopes[0]:
                print(f'{filename}:line {self.lineno}:Error:Porcedure returns non-existent type "{self.type}"')
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                ty = scopes[0][self.type][0]
                while isinstance(ty, str):    
                    ty = scopes[0][ty][0]
        else:
            ty = self.type

        if isinstance(ty, Array) or isinstance(ty, Struct):
            print(
                f'{filename}:line {self.lineno}:Error:Procedure cannot return aggregate type "{self.type}"')
            print(f'> {lines[self.lineno - 1]}')
            print(' ' * (self.col + 1) + '^')
            raise SyntaxError('Invalid return type')

        scopes.append([self.type])
        scopes.append(dict())
        for arg in self.args:
            if isinstance(arg.type, str):
                if arg.type not in scopes[0]:
                    print(f'{filename}:line {arg.lineno}:Error:Porcedure has a parametar of non-existnet type "{arg.type}"')
                    print(f'> {lines[arg.lineno - 1]}')
                    print(' ' * (arg.col + 1) + '^')
                    raise SyntaxError('Invalid type')
                else:
                    ty_arg = scopes[0][arg.type][0]
                    while isinstance(ty_arg, str):    
                        ty_arg = scopes[0][ty_arg][0]
            else:
                ty_arg = arg.type

            if arg.name in scopes[-1]:
                print(
                    f'{filename}:line {arg.lineno}:Error:Argument "{arg.name}" already given within the procdure {self.name}')
                print(f'> {lines[arg.lineno - 1]}')
                print(' ' * (arg.col + 1) + '^')
                raise SyntaxError('Argument redeclaration')
            elif isinstance(ty_arg, Array) or isinstance(ty_arg, Struct):
                print(
                    f'{filename}:line {arg.lineno}:Error:Procedure cannot have a paremeter of the aggregate type "{arg.type}"')
                print(f'> {lines[arg.lineno - 1]}')
                print(' ' * (arg.col + 1) + '^')
                raise SyntaxError('Invalid parameter type')
            else:
                scopes[-1][arg.name] = (arg.type, arg.lineno)

        has_return = self.body.type_check()
        scopes.pop()
        scopes.pop()

        if not isinstance(ty, VOID) and not has_return:
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

    def __str__(self):
        return 'block'

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

    def __str__(self):
        return f'{self.name} = {self.initial}'

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

        if isinstance(self.type, str):
            if self.type not in scopes[0]:
                print(f'{filename}:line {self.lineno}:Error:Variable declared with non-existent type "{self.type}"')
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                ty = scopes[0][self.type][0]
                while isinstance(ty, str):    
                    ty = scopes[0][ty][0]
        else:
            ty = self.type

        if isinstance(self.initial.type, str):
            if self.initial.type not in scopes[0]:
                print(f'{filename}:line {self.initial.lineno}:Error:Variable initialized with non-existent type "{self.initial.type}"')
                print(f'> {lines[self.initial.lineno - 1]}')
                print(' ' * (self.initial.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                ty_init = scopes[0][self.initial.type][0]
                while isinstance(ty_init, str):    
                    ty_init = scopes[0][ty_init][0]
        else:
            ty_init = self.initial.type

        if isinstance(ty, VOID):
            print(
                f'{filename}:line {self.lineno}:Error:Variable "{self.name}" cannot be declared as VOID')
            print(f'> {lines[self.lineno - 1]}')
            print(' ' * (self.col + 1) + '^')
            raise TypeError('Unexpected declaration type')

        elif isinstance(ty_init, VOID):
            print(
                f'{filename}:line {self.initial.lineno}:Error:Variable "{self.name}" cannot be initialized with expression of type VOID')
            print(f'> {lines[self.initial.lineno - 1]}')
            print(' ' * (self.initial.col + 1) + '^')
            raise TypeError('Unexpected expression type')

        if isinstance(ty, Array) or isinstance(ty, Struct):
            if not (isinstance(self.initial, Number) and self.initial.value == 0):
                print(
                    f'{filename}:line {self.initial.lineno}:Error:Unexpected intial value of a variable')
                print(
                    f'{filename}:line {self.initial.lineno}:Info:0 expected')
                print(f'> {lines[self.initial.lineno - 1]}')
                print(' ' * (self.initial.col + 1) + '^')
                raise ValueError("Invalid initial value")

        elif ty != ty_init:
            print(
                f'{filename}:line {self.initial.lineno}:Error:Variable "{self.name}" of type "{self.type}" initialized with expression of different type "{self.initial.type}"')
            print(f'> {lines[self.initial.lineno - 1]}')
            print(' ' * (self.initial.col + 1) + '^')
            raise TypeError('Unexpected expression type')

        scopes[-1][self.name] = (self.type, self.lineno)

        return False

    def to_tac(self):
        return {"var": self.name, "init": self.initial}


class Assign(Statment):
    def __init__(self, target, expr, lineno, col):
        self.target = target
        self.expr = expr
        self.lineno = lineno
        self.col = col

    def __str__(self):
        return f'{self.target} = {self.expr}'

    def type_check(self):
        self.target.type_check()
        self.expr.type_check()
        self.type = VOID()

        if isinstance(self.expr.type, str):
            if self.expr.type not in scopes[0]:
                print(f'{filename}:line {self.expr.lineno}:Error:Expression of non-existent type "{self.expr.type}"')
                print(f'> {lines[self.expr.lineno - 1]}')
                print(' ' * (self.expr.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                ty = scopes[0][self.expr.type][0]
                while isinstance(ty, str):    
                    ty = scopes[0][ty][0]
        else:
            ty = self.expr.type

        if isinstance(ty, VOID):
            print(
                f'{filename}:line {self.expr.lineno}:Error:Variable "{self.target.name}" cannot be assigned with expression of type VOID')
            print(f'> {lines[self.expr.lineno - 1]}')
            print(' ' * (self.expr.col + 1) + '^')
            raise TypeError('Unexpected declaration type')

        if self.target.type != self.expr.type:
            print(
                f'{filename}:line {self.expr.lineno}:Error:Variable "{self.target}" of type "{self.target.type}" assigned with expression of different type "{self.expr.type}"')
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

    def __str__(self):
        return 'while'

    def type_check(self):
        self.condition.type_check()

        if isinstance(self.condition.type, str):
            if self.condition.type not in scopes[0]:
                print(f'{filename}:line {self.condition.lineno}:Error:Expresion of non-existent type "{self.condition.type}"')
                print(f'> {lines[self.condition.lineno - 1]}')
                print(' ' * (self.condition.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                ty = scopes[0][self.condition.type][0]
                while isinstance(ty, str):    
                    ty = scopes[0][ty][0]
        else:
            ty = self.condition.type

        if not isinstance(ty, BOOL):
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

    def __str__(self):
        return 'if'

    def type_check(self):
        self.condition.type_check()

        if isinstance(self.condition.type, str):
            if self.condition.type not in scopes[0]:
                print(f'{filename}:line {self.condition.lineno}:Error:Expresion of non-existent type "{self.condition.type}"')
                print(f'> {lines[self.condition.lineno - 1]}')
                print(' ' * (self.condition.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                ty = scopes[0][self.condition.type][0]
                while isinstance(ty, str):    
                    ty = scopes[0][ty][0]
        else:
            ty = self.condition.type

        if not isinstance(ty, BOOL):
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
    
    def __str__(self):
        return f'{self.expr}'

    def type_check(self):
        self.expr.type_check()
        self.type = self.expr.type
        return False


class StructuredJump(Statment):
    def __init__(self, jump_type, lineno, col):
        self.jump_type = jump_type
        self.lineno = lineno
        self.col = col

    def __str__(self):
        return self.jump_type

    def type_check(self):
        return False


class Return(Statment):
    def __init__(self, expr, lineno, col):
        self.expr = expr
        self.lineno = lineno
        self.col = col
    
    def __str__(self):
        if self.expr is None:
            return 'return'
        else:
            return f'return {self.expr}'

    def type_check(self):
        if self.expr is not None:
            self.expr.type_check()
            self.type = self.expr.type

            if self.expr.type != scopes[1][0]:
                print(
                    f'{filename}:line {self.expr.lineno}:Error:Cannot return expression of type "{self.expr.type}"')
                print(
                    f'{filename}:line {self.lineno}:Info:Expected return type "{scopes[1][0]}"')
                print(f'> {lines[self.expr.lineno - 1]}')
                print(' ' * (self.expr.col + 1) + '^')
                raise TypeError('Invalid return type')
        else:
            self.type = VOID()

            if isinstance(scopes[1][0], str):
                ty = scopes[0][scopes[1][0]][0]
                while isinstance(ty, str):    
                    ty = scopes[0][ty][0]
            else:
                ty = scopes[1][0]

            if not isinstance(ty, VOID):
                print(
                    f'{filename}:line {self.expr.lineno}:Error:Cannot return expression void type when function requires "{scopes[1][0]}"')
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

    def __str__(self):
        return self.name

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
        self.col = col
        self.type = INT()

    def __str__(self):
        return str(self.value)

    def type_check(self):
        return


class Bool(Expr):
    def __init__(self, value, lineno, col):
        self.value = value
        self.lineno = lineno
        self.col = col
        self.type = BOOL()
    
    def __str__(self):
        return self.value

    def type_check(self):
        return


class Null(Expr):
    def __init__(self, lineno, col):
        self.lineno = lineno
        self.col = col
        self.type = Pointer(VOID())

    def __str__(self):
        return 'null'

    def type_check(self):
        return


class DeRef(Expr):
    def __init__(self, val, lineno, col):
        self.val = val
        self.lineno = lineno
        self.col = col

    def __str__(self):
        return f'*{self.val}'

    def type_check(self):
        self.val.type_check()

        if isinstance(self.val.type, str):
            if self.val.type not in scopes[0]:
                print(f'{filename}:line {self.val.lineno}:Error:Expresion of non-existent type "{self.val.type}"')
                print(f'> {lines[self.val.lineno - 1]}')
                print(' ' * (self.val.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                ty = scopes[0][self.val.type][0]
                while isinstance(ty, str):    
                    ty = scopes[0][ty][0]
        else:
            ty = self.val.type

        if not isinstance(ty, Pointer):
            print(
                f'{filename}:line {self.val.lineno}:Error:Pointer "{self.val}" has a wrong type of "{self.val.type}"')
            print(f'> {lines[self.val.lineno - 1]}')
            print(' ' * (self.val.col + 1) + '^')
            raise TypeError('Invalid type')

        if isinstance(self.val, Null):
            print(
                f'{filename}:line {self.val.lineno}:Error:Pointer "{self.val}" cannot be dereferenced')
            print(f'> {lines[self.val.lineno - 1]}')
            print(' ' * (self.val.col + 1) + '^')
            raise SyntaxError('Dereferencing null pointer')

        self.type = ty.type


class Reference(Expr):
    def __init__(self, val, lineno, col):
        self.val = val
        self.lineno = lineno
        self.col = col

    def __str__(self):
        return f'&{self.val}'

    def type_check(self):
        self.val.type_check()
        self.type = Pointer(self.val.type)


class Alloc(Expr):
    def __init__(self, value, type, lineno, col):
        self.value = value
        self.alloc_type = type
        self.lineno = lineno
        self.col = col
        self.type = Pointer(self.alloc_type)

    def __str__(self):
        return f'alloc {self.type}[{self.value}]'

    def type_check(self):
        if isinstance(self.alloc_type, str) and self.alloc_type not in scopes[0]:
            print(f'{filename}:line {self.lineno}:Error:Expresion given an non-existent type "{self.alloc_type}"')
            print(f'> {lines[self.lineno - 1]}')
            print(' ' * (self.col + 1) + '^')
            raise SyntaxError

        self.value.type_check()

        if isinstance(self.value.type, str):
            if self.value.type not in scopes[0]:
                print(f'{filename}:line {self.value.lineno}:Error:Expresion of non-existent type "{self.value.type}"')
                print(f'> {lines[self.value.lineno - 1]}')
                print(' ' * (self.value.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                ty = scopes[0][self.value.type][0]
                while isinstance(ty, str):    
                    ty = scopes[0][ty][0]
        else:
            ty = self.value.type

        if not isinstance(ty, INT):
            print(
                f'{filename}:line {self.value.lineno}:Error:Value in the brackets of unexpected of type "{self.value.type}"')
            print(f'> {lines[self.value.lineno - 1]}')
            print(' ' * (self.value.col + 1) + '^')
            raise TypeError('Invalid type')


class Access(Expr):
    def __init__(self, element, value, lineno, col):
        self.element = element
        self.value = value
        self.lineno = lineno
        self.col = col

    def __str__(self):
        return f'{self.element}[{self.value}]'

    def type_check(self):
        self.element.type_check()
        
        if isinstance(self.element.type, str):
            if self.element.type not in scopes[0]:
                print(f'{filename}:line {self.element.lineno}:Error:Expresion of non-existent type "{self.element.type}"')
                print(f'> {lines[self.element.lineno - 1]}')
                print(' ' * (self.element.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                el_type = scopes[0][self.element.type][0]
                while isinstance(el_type, str):    
                    el_type = scopes[0][el_type][0]
        else:
            el_type = self.element.type

        if not isinstance(el_type, Array) and not isinstance(el_type, Pointer):
            print(
                f'{filename}:line {self.element.lineno}:Error:Element "{self.element}" has an unexpected type of "{self.element.type}"')
            print(f'> {lines[self.element.lineno - 1]}')
            print(' ' * (self.element.col + 1) + '^')
            raise TypeError('Invalid type')

        self.type = el_type.type
        self.value.type_check()

        if isinstance(self.value.type, str):
            if self.value.type not in scopes[0]:
                print(f'{filename}:line {self.value.lineno}:Error:Expresion of non-existent type "{self.value.type}"')
                print(f'> {lines[self.value.lineno - 1]}')
                print(' ' * (self.value.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                val_ty = scopes[0][self.value.type][0]
                while isinstance(val_ty, str):    
                    val_ty = scopes[0][val_ty][0]
        else:
            val_ty = self.value.type

        if not isinstance(val_ty, INT):
            print(
                f'{filename}:line {self.value.lineno}:Error:Value "{self.value}" in the brackets of unexpected type of "{self.value.type}"')
            print(f'> {lines[self.value.lineno - 1]}')
            print(' ' * (self.value.col + 1) + '^')
            raise TypeError('Invalid type')


class Dot(Expr):
    def __init__(self, element, var, lineno, col):
        self.element = element
        self.var = var
        self.lineno = lineno
        self.col = col

    def __str__(self):
        return f'{self.element}.{self.var}'

    def type_check(self):
        self.element.type_check()

        if isinstance(self.element.type, str):
            if self.element.type not in scopes[0]:
                print(f'{filename}:line {self.element.lineno}:Error:Expresion of non-existent type "{self.element.type}"')
                print(f'> {lines[self.element.lineno - 1]}')
                print(' ' * (self.element.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                el_type = scopes[0][self.element.type][0]
                while isinstance(el_type, str):    
                    el_type = scopes[0][el_type][0]
        else:
            el_type = self.element.type

        if not isinstance(el_type, Struct):
            print(
                f'{filename}:line {self.element.lineno}:Error:Element "{self.element}" has an unexpected type of "{self.element.type}"')
            print(f'> {lines[self.element.lineno - 1]}')
            print(' ' * (self.element.col + 1) + '^')
            raise TypeError('Invalid type')

        for field in el_type.fields:
            if field[0].name == self.var.name:
                self.var.type = field[1]
                self.type = field[1]
                return
        else:
            print(
                f"{filename}:line {self.var.lineno}:Error:Identifier '{self.var.name}' not defined in struct")
            print(f'> {lines[self.var.lineno - 1]}')
            print(' ' * (self.var.col + 1) + '^')
            raise SyntaxError('Invalid identifier')


class To(Expr):
    def __init__(self, element, var, lineno, col):
        self.element = element
        self.var = var
        self.lineno = lineno
        self.col = col

    def __str__(self):
        return f'{self.element}->{self.var}'

    def type_check(self):
        self.element.type_check()

        if isinstance(self.element.type, str):
            if self.element.type not in scopes[0]:
                print(f'{filename}:line {self.element.lineno}:Error:Expresion of non-existent type "{self.element.type}"')
                print(f'> {lines[self.element.lineno - 1]}')
                print(' ' * (self.element.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                el_type = scopes[0][self.element.type][0]
                while isinstance(el_type, str):    
                    el_type = scopes[0][el_type][0]
        else:
            el_type = self.element.type
        
        try:
            if isinstance(el_type.type, str):
                if el_type.type not in scopes[0]:
                    print(f'{filename}:line {self.element.lineno}:Error:Expresion of non-existent type "{el_type.type}"')
                    print(f'> {lines[self.element.lineno - 1]}')
                    print(' ' * (self.element.col + 1) + '^')
                    raise SyntaxError('Invalid type')
                else:
                    el_root_type = scopes[0][el_type.type][0]
                    while isinstance(el_root_type, str):    
                        el_root_type = scopes[0][el_root_type][0]
            else:
                el_root_type = el_type.type
        except AttributeError:
            print(
                f'{filename}:line {self.element.lineno}:Error:Element "{self.element}" has an unexpected type of "{self.element.type}"')
            print(f'> {lines[self.element.lineno - 1]}')
            print(' ' * (self.element.col + 1) + '^')
            raise TypeError('Invalid type')


        if not (isinstance(el_type, Pointer) and isinstance(el_root_type, Struct)):
            print(
                f'{filename}:line {self.element.lineno}:Error:Element "{self.element}" has an unexpected type of "{self.element.type}"')
            print(f'> {lines[self.element.lineno - 1]}')
            print(' ' * (self.element.col + 1) + '^')
            raise TypeError('Invalid type')

        for field in el_root_type.fields:
            if field[0].name == self.var.name:
                self.var.type = field[1]
                self.type = field[1]
                return
        else:
            print(
                f"{filename}:line {self.var.lineno}:Error:Identifier '{self.var.name}' not defined in struct pointer")
            print(f'> {lines[self.var.lineno - 1]}')
            print(' ' * (self.var.col + 1) + '^')
            raise SyntaxError('Invalid identifier')


class UnopApp(Expr):
    def __init__(self, op, arg, lineno, col):
        self.op = op
        self.arg = arg
        self.lineno = lineno
        self.col = col
    
    def __str__(self):
        return f'{self.op}{self.arg}'

    def type_check(self):
        self.arg.type_check()

        if isinstance(self.arg.type, str):
            if self.arg.type not in scopes[0]:
                print(f'{filename}:line {self.arg.lineno}:Error:Expresion of non-existent type "{self.arg.type}"')
                print(f'> {lines[self.arg.lineno - 1]}')
                print(' ' * (self.arg.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                ty = scopes[0][self.arg.type][0]
                while isinstance(ty, str):    
                    ty = scopes[0][ty][0]
        else:
            ty = self.arg.type   

        if self.op in ['BITCOMPL', 'MINUS', 'UMINUS']:

            if isinstance(ty, INT):
                self.type = INT()
            else:
                print(
                    f"{filename}:line {self.arg.lineno}:Error:Operation and argumet's type '{self.arg.type}' not compatible")
                print(
                    f"{filename}:line {self.arg.lineno}:Info:Operation '{self.op}' requires argument of type INT")
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise TypeError('Invalid type')

        elif self.op == 'NOT':
            if isinstance(ty, BOOL):
                self.type = BOOL()
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

    def __str__(self):
        return f'{self.arg_left} {self.op} {self.arg_right}'

    def type_check(self):
        self.arg_left.type_check()
        self.arg_right.type_check()

        if isinstance(self.arg_left.type, str):
            if self.arg_left.type not in scopes[0]:
                print(f'{filename}:line {self.arg_left.lineno}:Error:Expresion of non-existent type "{self.arg_left.type}"')
                print(f'> {lines[self.arg_left.lineno - 1]}')
                print(' ' * (self.arg_left.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                ty_left = scopes[0][self.arg_left.type][0]
                while isinstance(ty_left, str):    
                    ty_left = scopes[0][ty_left][0]
        else:
            ty_left = self.arg_left.type   

        if isinstance(self.arg_right.type, str):
            if self.arg_right.type not in scopes[0]:
                print(f'{filename}:line {self.arg_right.lineno}:Error:Expresion of non-existent type "{self.arg_right.type}"')
                print(f'> {lines[self.arg_right.lineno - 1]}')
                print(' ' * (self.arg_right.col + 1) + '^')
                raise SyntaxError('Invalid type')
            else:
                ty_right = scopes[0][self.arg_right.type][0]
                while isinstance(ty_right, str):
                    ty_right = scopes[0][ty_right][0]
        else:
            ty_right = self.arg_right.type   

        if self.op in ['PLUS', 'MINUS', 'TIMES', 'DIV', 'MODULUS', 'BITSHR',
                       'BITSHL', 'BITXOR', 'BITOR', 'BITAND']:
            if isinstance(ty_left, INT) and isinstance(ty_right, INT):
                self.type = INT()
            else:
                print(
                    f"{filename}:line {self.arg_right.lineno}:Error:Operation and argumets' types not compatible")
                print(
                    f"{filename}:line {self.arg_right.lineno}:Info:For given opration '{self.op}' both arguments shall be INT'")
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise TypeError('Invalid arguments type')

        elif self.op in ['AND', 'OR']:
            if isinstance(ty_left, BOOL) and isinstance(ty_right, BOOL):
                self.type = BOOL()
            else:
                print(
                    f"{filename}:line {self.arg_right.lineno}:Error:Operation and argumets' types not compatible")
                print(
                    f"{filename}:line {self.arg_right.lineno}:Info:For given opration '{self.op}' both arguments shall be BOOL'")
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise TypeError('Invalid arguments type')

        elif self.op in ['EQUAL', 'DIFFERENT']:
            if (isinstance(ty_left, INT) and isinstance(ty_right, INT)) or (isinstance(ty_left, BOOL) and isinstance(ty_right, BOOL)) or (isinstance(ty_left, Pointer) and isinstance(ty_right, Pointer)):
                self.type = BOOL()
            else:
                print(
                    f"{filename}:line {self.arg_right.lineno}:Error:Operation and argumets' types not compatible")
                print(
                    f"{filename}:line {self.arg_right.lineno}:Info:For given opration '{self.op}' arguments shall be BOOL or INT'")
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise TypeError('Invalid arguments type')

        elif self.op in ['LESS', 'MORE', 'LESSEQ', 'MOREEQ']:
            if isinstance(ty_left, INT) and isinstance(ty_right, INT):
                self.type = BOOL()
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
    
    def __str__(self):
        return f'{self.function}()'

    def type_check(self):
        if self.function == 'print':
            self.type = VOID()
            if len(self.args) != 1:
                print(
                    f'{filename}:line {self.lineno}:Error:Function "print" can only have one argument, {len(self.args)} given')
                print(f'> {lines[self.lineno - 1]}')
                print(' ' * (self.col + 1) + '^')
                raise SyntaxError('Invalid argments')

            self.args[0].type_check()

            if isinstance(self.args[0].type, str):
                if self.args[0].type not in scopes[0]:
                    print(f'{filename}:line {self.args[0].lineno}:Error:Expresion of non-existent type "{self.args[0].type}"')
                    print(f'> {lines[self.args[0].lineno - 1]}')
                    print(' ' * (self.args[0].col + 1) + '^')
                    raise SyntaxError('Invalid type')
                else:
                    ty = scopes[0][self.args[0].type][0]
                    while isinstance(ty, str):
                        ty = scopes[0][ty][0]
            else:
                ty = self.args[0].type 
            
            if isinstance(ty, INT):
                self.function = '__bx_print_int'
            elif isinstance(ty, BOOL):
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

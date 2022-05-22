import json
import sys
import getopt
from create_ast import *
from custom_parser import Parser

filename = ''
next_temorary = 0
next_label = 0
operation_convert = {"PLUS": "add", "MINUS": "sub", "TIMES": "mul", "DIV": "div", "MODULUS": "mod", "BITAND": "and", "BITOR": "or", "BITXOR": "xor",
                     "BITSHL": "shl", "BITSHR": "shr", "UMINUS": "neg", "BITCOMPL": "not"}
jump_convert = {"EQUAL": 'jz', 'DIFFERENT': 'jnz', 'LESS': 'jl',
                'LESSEQ': 'jle', 'MORE': 'jnle', 'MOREEQ': 'jnl'}
__break_stack = []
__continue_stack = []


def evaluate_bool_expr(expression, tac):
    global next_temorary
    global next_label

    temp = '%' + str(next_temorary)
    next_temorary += 1
    Lt = "%.L" + str(next_label)
    next_label += 1
    Lf = "%.L" + str(next_label)
    next_label += 1

    tac[0]['body'].append(
        {'opcode': 'const', 'args': [0], 'result': temp})
    tmm_bool_expressions(expression, Lt, Lf, tac)
    tac[0]['body'].append(
        {'opcode': 'label', 'args': [Lt], 'result': None})
    tac[0]['body'].append(
        {'opcode': 'const', 'args': [1], 'result': temp})
    tac[0]['body'].append(
        {'opcode': 'label', 'args': [Lf], 'result': None})

    return temp


def tmm_bool_expressions(expression, Lt, Lf, tac):
    global next_temorary
    global next_label
    global filename

    if expression.type != 'bool':
        print(
            f'{filename}:line {expression.lineno}:Error:Unexpected expression type "{expression.type}"')
        print(
            f'{filename}:line {expression.lineno}:Info:BOOL expression type expected')
        raise ValueError('Wrong expression type')

    if isinstance(expression, Bool):
        if expression.value == 'true':
            tac[0]['body'].append(
                {"opcode": "jmp", "args": [Lt], "result": None})
        elif expression.value == 'false':
            tac[0]['body'].append(
                {"opcode": "jmp", "args": [Lf], "result": None})
        else:
            print(
                f'{filename}:line {expression.lineno}:Error:Unknown bool value "{expression.value}"')
            raise ValueError('Unknown value')

    elif isinstance(expression, Variable):
        for scope in reversed(scopes):
            if expression.name in scope:
                value = scope[expression.name][0]
                tac[0]['body'].append(
                    {'opcode': 'jz', 'args': [value, Lf], 'result': None})
                tac[0]['body'].append(
                    {'opcode': 'jmp', 'args': [Lt], 'result': None})
                
                return
        else:
            print(
                f"{filename}:line {expression.lineno}:Error:Undeclared variable '{expression.name}'")
            raise SyntaxError('Undeclared variable')

    elif isinstance(expression, UnopApp):
        if expression.arg.type == 'bool':
            tmm_bool_expressions(expression.arg, Lf, Lt, tac)
        elif expression.arg.type == 'int':  # if needed better to have sth for int to bool
            arg = '%' + str(next_temorary)
            next_temorary += 1

            op, args = tmm_int_expressions(expression.arg, tac)
            tac[0]["body"].append({"opcode": op, "args": args, "result": arg})

            tac[0]["body"].append(
                {"opcode": 'jz', 'args': [arg, Lt], 'result': None})
            tac[0]['body'].append(
                {"opcode": "jmp", 'args': [Lf], 'result': None})

        else:
            print(
                f'{filename}:line {expression.arg.lineno}:Error:Unknown expression type "{expression.arg.type}"')
            raise ValueError("Wrong expression type")

    elif isinstance(expression, BinopApp):
        if expression.op in set(jump_convert):
            arg1 = '%' + str(next_temorary)
            next_temorary += 1

            if expression.arg_left.type == 'int':
                op, args = tmm_int_expressions(expression.arg_left, tac)
                tac[0]["body"].append(
                    {"opcode": op, "args": args, "result": arg1})

            elif expression.arg_left.type == 'bool':
                temp = evaluate_bool_expr(expression.arg_left, tac)
                tac[0]["body"].append(
                    {"opcode": 'copy', 'args': [temp], "result": arg1})
            else:
                print(
                    f'{filename}:line {expression.arg_left.lineno}:Error:Unknown expression type "{expression.arg_left.type}"')
                raise ValueError("Wrong expression type")

            arg2 = '%' + str(next_temorary)
            next_temorary += 1

            if expression.arg_right.type == 'int':
                op, args = tmm_int_expressions(expression.arg_right, tac)
                tac[0]["body"].append(
                    {"opcode": op, "args": args, "result": arg2})

            elif expression.arg_right.type == 'bool':
                temp = evaluate_bool_expr(expression.arg_right, tac)
                tac[0]["body"].append(
                    {"opcode": 'copy', 'args': [temp], "result": arg2})
            else:
                print(
                    f'{filename}:line {expression.arg_right.lineno}:Error:Unknown expression type "{expression.arg_right.type}"')
                raise ValueError("Wrong expression type")

            tac[0]["body"].append(
                {'opcode': "sub", 'args': [arg1, arg2], "result": arg1})
            tac[0]["body"].append(
                {"opcode": jump_convert[expression.op], 'args': [arg1, Lt], 'result': None})
            tac[0]['body'].append(
                {"opcode": "jmp", 'args': [Lf], 'result': None})

        elif expression.op == 'AND':
            Li = "%.L" + str(next_label)
            next_label += 1

            tmm_bool_expressions(expression.arg_left, Li, Lf, tac)
            tac[0]['body'].append(
                {"opcode": 'label',  'args': [Li], 'result': None})
            tmm_bool_expressions(expression.arg_right, Lt, Lf, tac)

        elif expression.op == 'OR':
            Li = "%.L" + str(next_label)
            next_label += 1

            tmm_bool_expressions(expression.arg_left, Lt, Li, tac)
            tac[0]['body'].append(
                {"opcode": 'label',  'args': [Li], 'result': None})
            tmm_bool_expressions(expression.arg_right, Lt, Lf, tac)

        else:
            print(
                f'{filename}:line {expression.arg_right.lineno}:Error:Unknown binary opperation "{expression.op}"')
            raise SyntaxError("Unknown operation")

    else:
        print(
            f'{filename}:line {expression.lineno}:Error:Unrecognized expression "{expression}"')
        raise SyntaxError("Unknown expression")


def tmm_int_expressions(expression, tac):
    global next_temorary
    global filename

    if expression.type != 'int':
        print(
            f'{filename}:line {expression.lineno}:Error:Unexpected expression type "{expression.type}"')
        print(
            f'{filename}:line {expression.lineno}:Info:INT expression type expected')
        raise ValueError('Wrong expression type')

    if isinstance(expression, Number):
        return "const", [expression.value]

    elif isinstance(expression, Variable):
        for scope in reversed(scopes):
            if expression.name in scope:
                return "copy", [scope[expression.name][0]]
        else:
            print(
                f"{filename}:line {expression.lineno}:Error:Undeclared variable '{expression.name}'")
            raise SyntaxError('Undeclared variable')

    elif isinstance(expression, UnopApp):
        arg1 = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_int_expressions(expression.arg, tac)
        tac[0]["body"].append({"opcode": op, "args": args, "result": arg1})

        return operation_convert[expression.op], [arg1]

    elif isinstance(expression, BinopApp):
        arg1 = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_int_expressions(expression.arg_left, tac)
        tac[0]["body"].append({"opcode": op, "args": args, "result": arg1})

        arg2 = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_int_expressions(expression.arg_right, tac)
        tac[0]["body"].append({"opcode": op, "args": args, "result": arg2})

        return operation_convert[expression.op], [arg1, arg2]

    else:
        print(
            f'{filename}:line {expression.lineno}:Error:Unrecognized expression "{expression}"')
        raise SyntaxError("Unknown expression")


def tmm_statements(instruction, tac):
    global next_temorary
    global next_label
    global filename

    if isinstance(instruction, Block):
        scopes.append(dict())
        for stmt in instruction.body:
            tmm_statements(stmt, tac)
        scopes.pop()

    elif isinstance(instruction, VarDec):
        if instruction.name in scopes[-1]:
            print(
                f'{filename}:line {instruction.lineno}:Error:Redecalred variable "{instruction.name}" within the scope')
            print(
                f'{filename}:line {scopes[-1][instruction.name][1]}:Info:Declartion of variable "{instruction.name}"')
            raise SyntaxError('Variable redeclaration')

        result = '%' + str(next_temorary)
        next_temorary += 1

        if instruction.type == 'int':
            if instruction.initial.type != 'int':
                print(f'{filename}:line {instruction.initial.lineno}:Error:Variable "{instruction.name}" declared as int, initilaized with type "{instruction.initial.type}"')
                raise SyntaxError("Wrong type")

            op, args = tmm_int_expressions(instruction.initial, tac)
            tac[0]["body"].append(
                {"opcode": op, "args": args, "result": result})

        elif instruction.type == 'bool':
            if instruction.initial.type != 'bool':
                print(f'{filename}:line {instruction.initial.lineno}:Error:Variable "{instruction.name}" declared as bool, initilaized with type "{instruction.initial.type}"')
                raise SyntaxError("Wrong type")

            temp = evaluate_bool_expr(instruction.initial, tac)
            tac[0]['body'].append(
                {'opcode': 'copy', 'args': [temp], 'result': result})

        else:
            print(f'{filename}:line {instruction.lineno}:Error:Variable "{instruction.name}" has unknown type "{instruction.type}"')
            raise ValueError("Unknown type")

        scopes[-1][instruction.name] = (result, instruction.lineno)

    elif isinstance(instruction, Assign):
        for scope in reversed(scopes):
            if instruction.target.name in scope:

                result = scope[instruction.target.name][0]

                if instruction.type == 'int':
                    if instruction.expr.type != 'int':
                        print(
                            f'{filename}:line {instruction.expr.lineno}:Error:Variable "{instruction.target.name}" declared as int, assigned with type "{instruction.expr.type}"')
                        raise SyntaxError("Wrong type")

                    op, args = tmm_int_expressions(instruction.expr, tac)
                    tac[0]["body"].append(
                        {"opcode": op, "args": args, "result": result})

                elif instruction.type == 'bool':
                    if instruction.expr.type != 'bool':
                        print(
                            f'{filename}:line {instruction.expr.lineno}:Error:Variable "{instruction.target.name}" declared as bool, assigned with type "{instruction.expr.type}"')
                        raise SyntaxError("Wrong type")

                    temp = evaluate_bool_expr(instruction.expr, tac)
                    tac[0]['body'].append(
                        {'opcode': 'copy', 'args': [temp], 'result': result})

                else:
                    print(
                        f'{filename}:line {instruction.lineno}:Error:Variable "{instruction.name}" has unknown type "{instruction.type}"')
                    raise ValueError("Unknown type")

                return
        else:
            print(
                f'{filename}:line {instruction.target.lineno}:Error:Assigning an undeclared variable "{instruction.target.name}"')
            raise SyntaxError('Undeclared variable')

    elif isinstance(instruction, Call):
        result = '%' + str(next_temorary)
        next_temorary += 1

        if instruction.args[0].type == 'int':
            op, args = tmm_int_expressions(instruction.args[0], tac)
            tac[0]["body"].append(
                {"opcode": op, "args": args, "result": result})
        else:
            print(
                f'{filename}:line {instruction.args[0].lineno}:Error:Expression in print has unexpected type "{instruction.args[0].type}"')
            print(
                f'{filename}:line {instruction.args[0].lineno}:Info:INT type is accpeted for printing')
            raise ValueError("Wrong type")

        tac[0]["body"].append(
            {"opcode": instruction.function, "args": [result], "result": None})

    elif isinstance(instruction, If):
        Lt = "%.L" + str(next_label)
        next_label += 1
        Lf = "%.L" + str(next_label)
        next_label += 1

        tmm_bool_expressions(instruction.condition, Lt, Lf, tac)
        tac[0]["body"].append(
            {'opcode': 'label', 'args': [Lt], 'result': None})
        tmm_statements(instruction.instructions, tac)

        if instruction.else_case is None:
            tac[0]["body"].append(
                {'opcode': 'label', 'args': [Lf], 'result': None})
        else:
            Lo = "%.L" + str(next_label)
            next_label += 1
            tac[0]["body"].append(
                {'opcode': 'jmp', 'args': [Lo], 'result': None})
            tac[0]["body"].append(
                {'opcode': 'label', 'args': [Lf], 'result': None})
            tmm_statements(instruction.else_case, tac)
            tac[0]["body"].append(
                {'opcode': 'label', 'args': [Lo], 'result': None})

    elif isinstance(instruction, While):
        Lhead = "%.L" + str(next_label)
        next_label += 1
        tac[0]["body"].append(
            {'opcode': 'label', 'args': [Lhead], 'result': None})

        Lbod = "%.L" + str(next_label)
        next_label += 1
        Lend = "%.L" + str(next_label)
        next_label += 1

        __break_stack.append(Lend)
        __continue_stack.append(Lhead)
        tmm_bool_expressions(instruction.condition, Lbod, Lend, tac)
        tac[0]["body"].append(
            {'opcode': 'label', 'args': [Lbod], 'result': None})
        tmm_statements(instruction.instructions, tac)
        tac[0]["body"].append(
            {'opcode': 'jmp', 'args': [Lhead], 'result': None})
        tac[0]["body"].append(
            {'opcode': 'label', 'args': [Lend], 'result': None})
        __continue_stack.pop()
        __break_stack.pop()

    elif isinstance(instruction, StructuredJump):
        if instruction.jump_type == 'break':
            if len(__break_stack) == 0:
                print(
                    f'{filename}:line {instruction.lineno}:Error:Break instruction out of loop')
                raise SyntaxError('Misplaced instruction')

            tac[0]['body'].append(
                {'opcode': 'jmp', 'args': [__break_stack[-1]], 'result': None})

        elif instruction.jump_type == 'continue':
            if len(__continue_stack) == 0:
                print(
                    f'{filename}:line {instruction.lineno}:Error:Continue instruction out of loop')
                raise SyntaxError('Misplaced instruction')

            tac[0]['body'].append(
                {'opcode': 'jmp', 'args': [__continue_stack[-1]], 'result': None})

    else:
        print(
            f'{filename}:line {instruction.lineno}:Error:Unrecognized statement "{instruction}"')
        raise SyntaxError("Unknown statement")


def bx2tac(code, file=""):
    global filename
    filename = file

    parser = Parser(filename)
    try:
        instruction = parser.parse(code)
    except SyntaxError as err:
        print('Syntax error:', err)
        sys.exit(1)
    except ValueError as err:
        print('Value error:', err)
        sys.exit(1)

    tac = [{"proc": "@main", "body": []}]

    try:
        instruction.type_check()
        tmm_statements(instruction, tac)
        tac_json = json.dumps(tac)
        return tac_json
    except SyntaxError as err:
        print('Syntax error:', err)
        sys.exit(1)
    except ValueError as err:
        print('Value error:', err)
        sys.exit(1)


if __name__ == '__main__':
    opts, args = getopt.getopt(sys.argv[1:], '', [])
    filename = args[0]

    if not filename.endswith('.bx'):
        print(filename, 'Wrong file extention')
        raise TypeError

    with open(filename, 'r') as file:
        code = file.read()

    # print(bx2tac(code))
    with open(filename.split('\\')[-1].split('.')[0] + '.json', 'w') as file:
        tac_file = bx2tac(code, filename=filename)
        file.write(tac_file)

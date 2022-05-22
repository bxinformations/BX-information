import json
import sys
import getopt

from create_ast import *
from bx2front import bx2front

next_temorary = 0
next_label = 0
operation_convert = {"PLUS": "add", "MINUS": "sub", "TIMES": "mul", "DIV": "div", "MODULUS": "mod", "BITAND": "and", "BITOR": "or", "BITXOR": "xor",
                     "BITSHL": "shl", "BITSHR": "shr", "UMINUS": "neg", "BITCOMPL": "not"}
jump_convert = {"EQUAL": 'jz', 'DIFFERENT': 'jnz', 'LESS': 'jl',
                'LESSEQ': 'jle', 'MORE': 'jnle', 'MOREEQ': 'jnl'}
__break_stack = []
__continue_stack = []


def tmm_void_expression(expression, tac):
    global next_temorary

    if expression.type != 'void':
        print(
            f'{filename}:line {expression.lineno}:Error:Unexpected expression type "{expression.type}"')
        print(
            f'{filename}:line {expression.lineno}:Info:"void" expression type expected')
        print(f'> {lines[expression.lineno - 1]}')
        print(' ' * (expression.col + 1) + '^')
        raise TypeError('Unexpected expression type')

    if isinstance(expression, Call):
        for i, arg in enumerate(expression.args):
            result = '%' + str(next_temorary)
            next_temorary += 1

            if arg.type == 'int':
                op, args = tmm_int_expressions(arg, tac)
                tac["body"].append(
                    {"opcode": op, "args": args, "result": result})

            elif arg.type == 'bool':
                temp = evaluate_bool_expr(arg, tac)
                tac['body'].append(
                    {'opcode': 'copy', 'args': [temp], 'result': result})
            else:
                print(
                    f'{filename}:line {arg.lineno}:Error:Argument has unknown type "{arg.type}"')
                print(f'> {lines[arg.lineno - 1]}')
                print(' ' * (arg.col + 1) + '^')
                raise TypeError("Invalid type")

            if i == 6 and len(expression.args) & 1:
                tac["body"].append({'opcode': 'param', "args": [
                                   i + 1, result], 'result': None})

            if i >= 6 and len(expression.args) & 1:
                tac["body"].append({'opcode': 'param', "args": [
                                   i + 2, result], 'result': None})
            else:
                tac["body"].append({'opcode': 'param', "args": [
                                   i + 1, result], 'result': None})

        return "call", ["@" + expression.function, len(expression.args)]

    else:
        print(
            f'{filename}:line {expression.lineno}:Error:Unrecognized expression "{expression}"')
        print(f'> {lines[expression.lineno - 1]}')
        print(' ' * (expression.col + 1) + '^')
        raise ValueError("Unknown expression")


def evaluate_bool_expr(expression, tac):
    global next_temorary
    global next_label

    temp = '%' + str(next_temorary)
    next_temorary += 1

    if isinstance(expression, Call):
        expression.type = "int"
        op, args = tmm_int_expressions(expression, tac)
        tac["body"].append({'opcode': op, 'args': args, 'result': temp})
        expression = "bool"
    else:
        Lt = "%.L" + str(next_label)
        next_label += 1
        Lf = "%.L" + str(next_label)
        next_label += 1

        tac['body'].append(
            {'opcode': 'const', 'args': [0], 'result': temp})
        tmm_bool_expressions(expression, Lt, Lf, tac)
        tac['body'].append(
            {'opcode': 'label', 'args': [Lt], 'result': None})
        tac['body'].append(
            {'opcode': 'const', 'args': [1], 'result': temp})
        tac['body'].append(
            {'opcode': 'label', 'args': [Lf], 'result': None})

    return temp


def tmm_bool_expressions(expression, Lt, Lf, tac):
    global next_temorary
    global next_label

    if expression.type != 'bool':
        print(
            f'{filename}:line {expression.lineno}:Error:Unexpected expression type "{expression.type}"')
        print(
            f'{filename}:line {expression.lineno}:Info:"bool" expression type expected')
        print(f'> {lines[expression.lineno - 1]}')
        print(' ' * (expression.col + 1) + '^')
        raise TypeError('Unexpected expression type')

    if isinstance(expression, Bool):
        if expression.value == 'true':
            tac['body'].append(
                {"opcode": "jmp", "args": [Lt], "result": None})
        elif expression.value == 'false':
            tac['body'].append(
                {"opcode": "jmp", "args": [Lf], "result": None})
        else:
            print(
                f'{filename}:line {expression.lineno}:Error:Unknown bool value "{expression.value}"')
            print(f'> {lines[expression.lineno - 1]}')
            print(' ' * (expression.col + 1) + '^')
            raise ValueError('Unknown value')

    elif isinstance(expression, Variable):
        for scope in reversed(scopes):
            if expression.name in scope:
                value = scope[expression.name][0]
                tac['body'].append(
                    {'opcode': 'jz', 'args': [value, Lf], 'result': None})
                tac['body'].append(
                    {'opcode': 'jmp', 'args': [Lt], 'result': None})
                break

    elif isinstance(expression, UnopApp):
        if expression.arg.type == 'bool':
            tmm_bool_expressions(expression.arg, Lf, Lt, tac)
        elif expression.arg.type == 'int':
            arg = '%' + str(next_temorary)
            next_temorary += 1

            op, args = tmm_int_expressions(expression.arg, tac)
            tac["body"].append({"opcode": op, "args": args, "result": arg})

            tac["body"].append(
                {"opcode": 'jz', 'args': [arg, Lt], 'result': None})
            tac['body'].append(
                {"opcode": "jmp", 'args': [Lf], 'result': None})

    elif isinstance(expression, BinopApp):
        if expression.op in set(jump_convert):
            arg1 = '%' + str(next_temorary)
            next_temorary += 1

            if expression.arg_left.type == 'int':
                op, args = tmm_int_expressions(expression.arg_left, tac)
                tac["body"].append(
                    {"opcode": op, "args": args, "result": arg1})

            elif expression.arg_left.type == 'bool':
                temp = evaluate_bool_expr(expression.arg_left, tac)
                tac["body"].append(
                    {"opcode": 'copy', 'args': [temp], "result": arg1})

            arg2 = '%' + str(next_temorary)
            next_temorary += 1

            if expression.arg_right.type == 'int':
                op, args = tmm_int_expressions(expression.arg_right, tac)
                tac["body"].append(
                    {"opcode": op, "args": args, "result": arg2})

            elif expression.arg_right.type == 'bool':
                temp = evaluate_bool_expr(expression.arg_right, tac)
                tac["body"].append(
                    {"opcode": 'copy', 'args': [temp], "result": arg2})

            tac["body"].append(
                {'opcode': "sub", 'args': [arg1, arg2], "result": arg1})
            tac["body"].append(
                {"opcode": jump_convert[expression.op], 'args': [arg1, Lt], 'result': None})
            tac['body'].append(
                {"opcode": "jmp", 'args': [Lf], 'result': None})

        elif expression.op == 'AND':
            Li = "%.L" + str(next_label)
            next_label += 1

            tmm_bool_expressions(expression.arg_left, Li, Lf, tac)
            tac['body'].append(
                {"opcode": 'label',  'args': [Li], 'result': None})
            tmm_bool_expressions(expression.arg_right, Lt, Lf, tac)

        elif expression.op == 'OR':
            Li = "%.L" + str(next_label)
            next_label += 1

            tmm_bool_expressions(expression.arg_left, Lt, Li, tac)
            tac['body'].append(
                {"opcode": 'label',  'args': [Li], 'result': None})
            tmm_bool_expressions(expression.arg_right, Lt, Lf, tac)

        else:
            print(
                f'{filename}:line {expression.lineno}:Error:Unknown binary opperation "{expression.op}"')
            print(f'> {lines[expression.lineno - 1]}')
            print(' ' * (expression.col + 1) + '^')
            raise SyntaxError("Unknown operation")

    elif isinstance(expression, Call):
        temp = '%' + str(next_temorary)
        next_temorary += 1

        for i, arg in enumerate(expression.args):
            result = '%' + str(next_temorary)
            next_temorary += 1

            if arg.type == 'int':
                op, args = tmm_int_expressions(arg, tac)
                tac["body"].append(
                    {"opcode": op, "args": args, "result": result})

            elif arg.type == 'bool':
                temp = evaluate_bool_expr(arg, tac)
                tac['body'].append(
                    {'opcode': 'copy', 'args': [temp], 'result': result})

            if i == 6 and len(expression.args) & 1:
                tac["body"].append({'opcode': 'param', "args": [
                                   i + 1, result], 'result': None})

            if i >= 6 and len(expression.args) & 1:
                tac["body"].append({'opcode': 'param', "args": [
                                   i + 2, result], 'result': None})
            else:
                tac["body"].append({'opcode': 'param', "args": [
                                   i + 1, result], 'result': None})

        tac["body"].append({"opcode": "call", 'args': [
                           "@" + expression.function, len(expression.args)], 'result': temp})
        tac['body'].append(
            {'opcode': 'jz', 'args': [temp, Lf], 'result': None})
        tac['body'].append({'opcode': 'jmp', 'args': [Lt], 'result': None})

    else:
        print(
            f'{filename}:line {expression.lineno}:Error:Unrecognized expression "{expression}"')
        print(f'> {lines[expression.lineno - 1]}')
        print(' ' * (expression.col + 1) + '^')
        raise ValueError("Unknown expression")


def tmm_int_expressions(expression, tac):
    global next_temorary

    if expression.type != 'int':
        print(
            f'{filename}:line {expression.lineno}:Error:Unexpected expression type "{expression.type}"')
        print(
            f'{filename}:line {expression.lineno}:Info:"int" expression type expected')
        print(f'> {lines[expression.lineno - 1]}')
        print(' ' * (expression.col + 1) + '^')
        raise TypeError('Unexpected expression type')

    if isinstance(expression, Number):
        return "const", [expression.value]

    elif isinstance(expression, Variable):
        for scope in reversed(scopes):
            if expression.name in scope:
                return "copy", [scope[expression.name][0]]

    elif isinstance(expression, UnopApp):
        arg1 = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_int_expressions(expression.arg, tac)
        tac["body"].append({"opcode": op, "args": args, "result": arg1})

        return operation_convert[expression.op], [arg1]

    elif isinstance(expression, BinopApp):
        arg1 = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_int_expressions(expression.arg_left, tac)
        tac["body"].append({"opcode": op, "args": args, "result": arg1})

        arg2 = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_int_expressions(expression.arg_right, tac)
        tac["body"].append({"opcode": op, "args": args, "result": arg2})

        return operation_convert[expression.op], [arg1, arg2]

    elif isinstance(expression, Call):
        for i, arg in enumerate(expression.args):
            result = '%' + str(next_temorary)
            next_temorary += 1

            if arg.type == 'int':
                op, args = tmm_int_expressions(arg, tac)
                tac["body"].append(
                    {"opcode": op, "args": args, "result": result})

            elif arg.type == 'bool':
                temp = evaluate_bool_expr(arg, tac)
                tac['body'].append(
                    {'opcode': 'copy', 'args': [temp], 'result': result})

            if i == 6 and len(expression.args) & 1:
                tac["body"].append({'opcode': 'param', "args": [
                                   i + 1, result], 'result': None})

            if i >= 6 and len(expression.args) & 1:
                tac["body"].append({'opcode': 'param', "args": [
                                   i + 2, result], 'result': None})
            else:
                tac["body"].append({'opcode': 'param', "args": [
                                   i + 1, result], 'result': None})

        return "call", ["@" + expression.function, len(expression.args)]

    else:
        print(
            f'{filename}:line {expression.lineno}:Error:Unrecognized expression "{expression}"')
        print(f'> {lines[expression.lineno - 1]}')
        print(' ' * (expression.col + 1) + '^')
        raise ValueError("Unknown expression")


def tmm_statements(instruction, tac):
    global next_temorary
    global next_label

    if isinstance(instruction, Block):
        scopes.append(dict())
        for stmt in instruction.body:
            if not isinstance(stmt, Statment):
                for s in stmt:
                    tmm_statements(s, tac)
            else:
                tmm_statements(stmt, tac)
        scopes.pop()

    elif isinstance(instruction, VarDec):
        result = '%' + str(next_temorary)
        next_temorary += 1

        if instruction.type == 'int':
            op, args = tmm_int_expressions(instruction.initial, tac)
            tac["body"].append(
                {"opcode": op, "args": args, "result": result})

        elif instruction.type == 'bool':
            temp = evaluate_bool_expr(instruction.initial, tac)
            tac['body'].append(
                {'opcode': 'copy', 'args': [temp], 'result': result})

        scopes[-1][instruction.name] = (result, instruction.lineno)

    elif isinstance(instruction, Assign):
        for scope in reversed(scopes):
            if instruction.target.name in scope:

                result = scope[instruction.target.name][0]

                if instruction.type == 'int':
                    op, args = tmm_int_expressions(instruction.expr, tac)
                    tac["body"].append(
                        {"opcode": op, "args": args, "result": result})

                elif instruction.type == 'bool':
                    temp = evaluate_bool_expr(instruction.expr, tac)
                    tac['body'].append(
                        {'opcode': 'copy', 'args': [temp], 'result': result})
                break

    elif isinstance(instruction, If):
        Lt = "%.L" + str(next_label)
        next_label += 1
        Lf = "%.L" + str(next_label)
        next_label += 1

        tmm_bool_expressions(instruction.condition, Lt, Lf, tac)
        tac["body"].append(
            {'opcode': 'label', 'args': [Lt], 'result': None})
        tmm_statements(instruction.instructions, tac)

        if instruction.else_case is None:
            tac["body"].append(
                {'opcode': 'label', 'args': [Lf], 'result': None})
        else:
            Lo = "%.L" + str(next_label)
            next_label += 1
            tac["body"].append(
                {'opcode': 'jmp', 'args': [Lo], 'result': None})
            tac["body"].append(
                {'opcode': 'label', 'args': [Lf], 'result': None})
            tmm_statements(instruction.else_case, tac)
            tac["body"].append(
                {'opcode': 'label', 'args': [Lo], 'result': None})

    elif isinstance(instruction, While):
        Lhead = "%.L" + str(next_label)
        next_label += 1
        tac["body"].append(
            {'opcode': 'label', 'args': [Lhead], 'result': None})

        Lbod = "%.L" + str(next_label)
        next_label += 1
        Lend = "%.L" + str(next_label)
        next_label += 1

        __break_stack.append(Lend)
        __continue_stack.append(Lhead)
        tmm_bool_expressions(instruction.condition, Lbod, Lend, tac)
        tac["body"].append(
            {'opcode': 'label', 'args': [Lbod], 'result': None})
        tmm_statements(instruction.instructions, tac)
        tac["body"].append(
            {'opcode': 'jmp', 'args': [Lhead], 'result': None})
        tac["body"].append(
            {'opcode': 'label', 'args': [Lend], 'result': None})
        __continue_stack.pop()
        __break_stack.pop()

    elif isinstance(instruction, StructuredJump):
        if instruction.jump_type == 'break':
            if len(__break_stack) == 0:
                print(
                    f'{filename}:line {instruction.lineno}:Error:Break instruction out of loop')
                print(f'> {lines[instruction.lineno - 1]}')
                print(' ' * (instruction.col + 1) + '^')
                raise SyntaxError('Misplaced instruction')

            tac['body'].append(
                {'opcode': 'jmp', 'args': [__break_stack[-1]], 'result': None})

        elif instruction.jump_type == 'continue':
            if len(__continue_stack) == 0:
                print(
                    f'{filename}:line {instruction.lineno}:Error:Continue instruction out of loop')
                print(f'> {lines[instruction.lineno - 1]}')
                print(' ' * (instruction.col + 1) + '^')
                raise SyntaxError('Misplaced instruction')

            tac['body'].append(
                {'opcode': 'jmp', 'args': [__continue_stack[-1]], 'result': None})

    elif isinstance(instruction, Eval):
        if instruction.type == 'int':
            op, args = tmm_int_expressions(instruction.expr, tac)
            tac["body"].append(
                {"opcode": op, "args": args, "result": None})
        elif instruction.type == 'bool':
            temp = evaluate_bool_expr(instruction.expr, tac)
            tac['body'].append(
                {'opcode': 'copy', 'args': [temp], 'result': None})
        elif instruction.type == "void":
            op, args = tmm_void_expression(instruction.expr, tac)
            tac["body"].append(
                {"opcode": op, "args": args, "result": None})

    elif isinstance(instruction, Return):
        if instruction.type != "void":
            result = '%' + str(next_temorary)
            next_temorary += 1
            if instruction.type == 'int':
                op, args = tmm_int_expressions(instruction.expr, tac)
                tac["body"].append(
                    {"opcode": op, "args": args, "result": result})

            elif instruction.type == 'bool':
                temp = evaluate_bool_expr(instruction.expr, tac)
                tac['body'].append(
                    {'opcode': 'copy', 'args': [temp], 'result': result})

            tac["body"].append(
                {"opcode": "ret", "args": [result], "result": None})
        else:
            tac["body"].append({"opcode": "ret", "args": [], "result": None})

    else:
        print(
            f'{filename}:line {instruction.lineno}:Error:Unrecognized statement "{instruction}"')
        print(f'> {lines[instruction.lineno - 1]}')
        print(' ' * (instruction.col + 1) + '^')
        raise ValueError("Unknown statement")


def tmm_globs(instructions):
    tac = []
    index = []
    for instruction in instructions:
        if isinstance(instruction, ProcDec):
            index.append((len(tac), instruction))
            tac.append({"proc": "@" + instruction.name,
                       "args": ["%" + arg.name for arg in instruction.args], "body": []})
            scopes[0][instruction.name] = (
                "@" + instruction.name, instruction.lineno)
        else:
            for var in instruction:
                if var.type == 'int':
                    tac.append(
                        {"var": "@" + var.name, "init": var.initial.value})
                else:
                    tac.append(
                        {"var": "@" + var.name, "init": 0 if var.initial.value == 'false' else 1})

                scopes[0][var.name] = ("@" + var.name, var.lineno)

    return tac, index


def bx2tac(code, file=""):
    global filename
    filename = file
    for line in code.split('\n'):
        lines.append(line)

    instructions = bx2front(code, filename)
    tac, index = tmm_globs(instructions)

    try:
        for i, instruction in index:
            if isinstance(instruction, ProcDec):
                scopes.append({arg.name: ("%" + arg.name, arg.lineno)
                              for arg in instruction.args})
                tmm_statements(instruction.body, tac[i])
                scopes.pop()

        tac_json = json.dumps(tac)
    except SyntaxError as err:
        print('Syntax error:', err)
        sys.exit(1)
    except TypeError as err:
        print('Type error:', err)
        sys.exit(1)
    except ValueError as err:
        print('Value error:', err)
        sys.exit(1)

    return tac_json


if __name__ == '__main__':
    opts, args = getopt.getopt(sys.argv[1:], '', [])
    filename = args[0]

    if not filename.endswith('.bx'):
        print(filename, 'Wrong file extention')
        raise TypeError

    with open(filename, 'r') as file:
        code = file.read()

    with open(filename.split('\\')[-1].split('.')[0] + '.json', 'w') as file:
        tac_file = bx2tac(code, file=filename)
        file.write(tac_file)

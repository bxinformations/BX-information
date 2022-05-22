import json
from re import L
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

def tmm_bool_expressions(expression, Lt, Lf, tac):
    global next_temorary
    global next_label

    if isinstance(expression.type, str):
        expr_type = scopes[0][expression.type][0]
        while isinstance(expr_type, str):
            expr_type = scopes[0][expr_type][0]
    else:
        expr_type = expression.type

    if not isinstance(expr_type, BOOL):
        print(
            f'{filename}:line {expression.lineno}:Error:Unexpected expression type "{expr_type}"')
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

    elif isinstance(expression, DeRef):
        res = '%' + str(next_temorary)
        next_temorary += 1
        temp = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_expressions(expression.val, tac)
        if op == 'load':
            tac['body'].append(
                {'opcode': 'getaddress_array', 'args': args, 'result': temp})
        elif op == 'copy':
            temp = args[0]
        else:
            tac['body'].append({'opcode': op, 'args': args, 'result': temp})

        tac['body'].append({"opcode": "load", "args": [
                           temp, 0, 1, 0], "result": res})

        tac['body'].append(
            {'opcode': 'jz', 'args': [res, Lf], 'result': None})
        tac['body'].append(
            {'opcode': 'jmp', 'args': [Lt], 'result': None})

    elif isinstance(expression, Access):
        res = '%' + str(next_temorary)
        next_temorary += 1
        element = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_expressions(expression.element, tac)
        if op == 'load':
            tac['body'].append(
                {'opcode': 'getaddress_array', 'args': args, 'result': element})
        elif op == 'copy':
            element = args[0]
        else:
            tac['body'].append({'opcode': op, 'args': args, 'result': element})

        offset = '%' + str(next_temorary)
        next_temorary += 1

        index = '%' + str(next_temorary)
        next_temorary += 1
        op, args = tmm_expressions(expression.value, tac)
        tac['body'].append({'opcode': op, 'args': args, 'result': index})

        size = '%' + str(next_temorary)
        next_temorary += 1
        tac['body'].append({'opcode': 'const', 'args': [
                           expr_type.sizeof()], 'result': size})

        tac['body'].append(
            {'opcode': 'mul', 'args': [index, size], 'result': offset})

        tac['body'].append({"opcode": "load", "args": [
                           element, 0, 1, offset], "result": res})

        tac['body'].append(
            {'opcode': 'jz', 'args': [res, Lf], 'result': None})
        tac['body'].append(
            {'opcode': 'jmp', 'args': [Lt], 'result': None})

    elif isinstance(expression, Dot):
        res = '%' + str(next_temorary)
        next_temorary += 1

        temp = '%' + str(next_temorary)
        next_temorary += 1
        op, args = tmm_expressions(expression.element, tac)
        if op == 'load':
            tac['body'].append(
                {'opcode': 'getaddress_array', 'args': args, 'result': temp})
        elif op == 'copy':
            temp = args[0]
        else:
            tac['body'].append({'opcode': op, 'args': args, 'result': temp})

        offset = 0
        if isinstance(expression.element.type, str):
            el_type = scopes[0][expression.element.type][0]
            while isinstance(el_type, str):
                el_type = scopes[0][el_type][0]
        else:
            el_type = expression.element.type

        for _, field in enumerate(el_type.fields):
            if field[0].name == expression.var.name:
                tac['body'].append({'opcode': 'load', 'args': [
                    temp, 0, 1, offset], 'result': res})
                break
            if isinstance(field[1], str):
                fl_type = scopes[0][field[1]][0]
                while isinstance(fl_type, str):
                    fl_type = scopes[0][fl_type][0]
            else:
                fl_type = field[1]
            offset += fl_type.sizeof()

        tac['body'].append(
            {'opcode': 'jz', 'args': [res, Lf], 'result': None})
        tac['body'].append(
            {'opcode': 'jmp', 'args': [Lt], 'result': None})

    elif isinstance(expression, To):
        res = '%' + str(next_temorary)
        next_temorary += 1

        temp = '%' + str(next_temorary)
        next_temorary += 1

        deref = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_expressions(expression.element, tac)
        op_prev = op
        if op == 'load':
            tac['body'].append(
                {'opcode': 'getaddress_array', 'args': args, 'result': deref})
            op_prev = 'getaddress_array'
        elif op == 'copy':
            deref = args[0]
        else:
            tac['body'].append({'opcode': op, 'args': args, 'result': deref})
        
        op, args = 'load', [deref, 0, 1, 0]

        if op_prev == 'getaddress_array':
            struct = '%' + str(next_temorary)
            next_temorary += 1
            tac['body'].append({'opcode': op, 'args' : args, 'result': struct})
            args = [struct, 0, 1, 0]
            op = 'load'

        if op == 'load':
            tac['body'].append(
                {'opcode': 'getaddress_array', 'args': args, 'result': temp})
        elif op == 'copy':
            temp = args[0]
        else:
            tac['body'].append({'opcode': op, 'args': args, 'result': temp})
        
        if isinstance(expression.element.type, str):
            expr_el_ty = scopes[0][expression.element.type][0]
            while isinstance(expr_el_ty, str):
                expr_el_ty = scopes[0][expr_el_ty][0]
        else:
            expr_el_ty = expression.element.type
        
        if isinstance(expr_el_ty.type, str):
            root_ty = scopes[0][expr_el_ty.type][0]
            while isinstance(root_ty, str):
                root_ty = scopes[0][root_ty][0]
        else:
            root_ty = expr_el_ty.type

        offset = 0
        for _, field in enumerate(root_ty.fields):
            if field[0].name == expression.var.name:
                tac['body'].append({'opcode': 'load', 'args': [
                    temp, 0, 1, offset], 'result': res})
                break

            if isinstance(field[1], str):
                fl_type = scopes[0][field[1]][0]
                while isinstance(fl_type, str):
                    fl_type = scopes[0][fl_type][0]
            else:
                fl_type = field[1]

            offset += fl_type.sizeof()

        tac['body'].append(
            {'opcode': 'jz', 'args': [res, Lf], 'result': None})
        tac['body'].append(
            {'opcode': 'jmp', 'args': [Lt], 'result': None})

    elif isinstance(expression, UnopApp):
        tmm_bool_expressions(expression.arg, Lf, Lt, tac)

    elif isinstance(expression, BinopApp):
        if expression.op in set(jump_convert):
            arg1 = '%' + str(next_temorary)
            next_temorary += 1

            if isinstance(expression.arg_left.type, str):
                left_ty = scopes[0][expression.arg_left.type][0]
                while isinstance(left_ty, str):
                    left_ty = scopes[0][left_ty][0]
            else:
                left_ty = expression.arg_left.type

            op, args = evaluate_bool_expr(expression.arg_left, tac) if isinstance(left_ty, BOOL) else  tmm_expressions(expression.arg_left, tac)
            tac["body"].append(
                {"opcode": op, "args": args, "result": arg1})

            arg2 = '%' + str(next_temorary)
            next_temorary += 1
            op, args = evaluate_bool_expr(expression.arg_right, tac) if isinstance(left_ty, BOOL) else tmm_expressions(expression.arg_right, tac)
            tac["body"].append(
                {"opcode": op, "args": args, "result": arg2})

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

            if isinstance(arg.type, str):
                arg_ty = scopes[0][arg.type][0]
                while isinstance(arg_ty, str):
                    arg_ty = scopes[0][arg_ty][0]
            else:
                arg_ty = arg.type

            op, args = evaluate_bool_expr(arg, tac) if isinstance(arg_ty, BOOL) else tmm_expressions(arg, tac)
            tac["body"].append(
                {"opcode": op, "args": args, "result": result})

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


def evaluate_bool_expr(expression, tac):
    global next_temorary
    global next_label

    if isinstance(expression, Call):
        return tmm_expressions(expression, tac)

    temp = '%' + str(next_temorary)
    next_temorary += 1

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

    return "copy", [temp]


def tmm_expressions(expression, tac):
    global next_temorary

    if isinstance(expression.type, str):
        expr_type = scopes[0][expression.type][0]
        while isinstance(expr_type, str):
            expr_type = scopes[0][expr_type][0]
    else:
        expr_type = expression.type

    if isinstance(expression, Call):
        for i, arg in enumerate(expression.args):
            result = '%' + str(next_temorary)
            next_temorary += 1

            if isinstance(arg.type, str):
                arg_ty = scopes[0][arg.type][0]
                while isinstance(arg_ty, str):
                    arg_ty = scopes[0][arg_ty][0]
            else:
                arg_ty = arg.type

            op, args = evaluate_bool_expr(arg, tac) if isinstance(arg_ty, BOOL) else tmm_expressions(arg, tac)
            tac["body"].append(
                {"opcode": op, "args": args, "result": result})

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

    elif isinstance(expression, Alloc):
        if isinstance(expression.alloc_type, str):
            ty = scopes[0][expression.alloc_type][0]
            while isinstance(ty, str):
                ty = scopes[0][ty][0]
        else:
            ty = expression.alloc_type

        calloc = Call("calloc", [expression.value, Number(
            ty.sizeof(), None, None)], None, None)
        calloc.type = expression.type

        return tmm_expressions(calloc, tac)

    elif isinstance(expression, Number):
        return "const", [expression.value]

    elif isinstance(expression, Null):
        return "copy", ["null"]

    elif isinstance(expression, Variable):
        for scope in reversed(scopes):
            if expression.name in scope:
                return "copy", [scope[expression.name][0]]

    elif isinstance(expression, DeRef):
        temp = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_expressions(expression.val, tac)
        if op == 'load':
            tac['body'].append(
                {'opcode': 'getaddress_array', 'args': args, 'result': temp})
        elif op == 'copy':
            temp = args[0]
        else:
            tac['body'].append({'opcode': op, 'args': args, 'result': temp})

        return "load", [temp, 0, 1, 0]

    elif isinstance(expression, Access):
        element = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_expressions(expression.element, tac)
        if op == 'load':
            tac['body'].append(
                {'opcode': 'getaddress_array', 'args': args, 'result': element})
        elif op == 'copy':
            element = args[0]
        else:
            tac['body'].append({'opcode': op, 'args': args, 'result': element})

        offset = '%' + str(next_temorary)
        next_temorary += 1

        index = '%' + str(next_temorary)
        next_temorary += 1
        op, args = tmm_expressions(expression.value, tac)
        tac['body'].append({'opcode': op, 'args': args, 'result': index})

        size = '%' + str(next_temorary)
        next_temorary += 1
        tac['body'].append({'opcode': 'const', 'args': [
                           expr_type.sizeof()], 'result': size})

        tac['body'].append(
            {'opcode': 'mul', 'args': [index, size], 'result': offset})

        return 'load', [element, 0, 1, offset]

    elif isinstance(expression, Dot):
        temp = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_expressions(expression.element, tac)

        if op == 'load':
            tac['body'].append(
                {'opcode': 'getaddress_array', 'args': args, 'result': temp})
        elif op == 'copy':
            temp = args[0]
        else:
            tac['body'].append({'opcode': op, 'args': args, 'result': temp})   

        if isinstance(expression.element.type, str):
            el_type = scopes[0][expression.element.type][0]
            while isinstance(el_type, str):
                el_type = scopes[0][el_type][0]
        else:
            el_type = expression.element.type

        offset = 0
        for _, field in enumerate(el_type.fields):
            if field[0].name == expression.var.name:
                return 'load', [temp, 0, 1, offset]

            if isinstance(field[1], str):
                fl_type = scopes[0][field[1]][0]
                while isinstance(fl_type, str):
                    fl_type = scopes[0][fl_type][0]
            else:
                fl_type = field[1]

            offset += fl_type.sizeof()

    elif isinstance(expression, To):
        temp = '%' + str(next_temorary)
        next_temorary += 1

        deref = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_expressions(expression.element, tac)
        op_prev = op
        if op == 'load':
            tac['body'].append(
                {'opcode': 'getaddress_array', 'args': args, 'result': deref})
            op_prev = 'getaddress_array'
        elif op == 'copy':
            deref = args[0]
        else:
            tac['body'].append({'opcode': op, 'args': args, 'result': deref})
        
        op, args = 'load', [deref, 0, 1, 0]

        if op_prev == 'getaddress_array':
            struct = '%' + str(next_temorary)
            next_temorary += 1
            tac['body'].append({'opcode': op, 'args' : args, 'result': struct})
            args = [struct, 0, 1, 0]
            op = 'load'

        if op == 'load':
            tac['body'].append(
                {'opcode': 'getaddress_array', 'args': args, 'result': temp})
        elif op == 'copy':
            temp = args[0]
        else:
            tac['body'].append({'opcode': op, 'args': args, 'result': temp})
        
        if isinstance(expression.element.type, str):
            expr_el_ty = scopes[0][expression.element.type][0]
            while isinstance(expr_el_ty, str):
                expr_el_ty = scopes[0][expr_el_ty][0]
        else:
            expr_el_ty = expression.element.type
        
        if isinstance(expr_el_ty.type, str):
            root_ty = scopes[0][expr_el_ty.type][0]
            while isinstance(root_ty, str):
                root_ty = scopes[0][root_ty][0]
        else:
            root_ty = expr_el_ty.type

        offset = 0
        for _, field in enumerate(root_ty.fields):
            if field[0].name == expression.var.name:
                return 'load', [temp, 0, 1, offset]

            if isinstance(field[1], str):
                fl_type = scopes[0][field[1]][0]
                while isinstance(fl_type, str):
                    fl_type = scopes[0][fl_type][0]
            else:
                fl_type = field[1]

            offset += fl_type.sizeof()
        
    elif isinstance(expression, Reference):
        _, args = tmm_expressions(expression.val, tac)

        if isinstance(expression.val.type, str):
            val_ty = scopes[0][expression.val.type][0]
            while isinstance(val_ty, str):
                val_ty = scopes[0][val_ty][0]
        else:
            val_ty = expression.val.type

        if isinstance(expression.val, Variable) and (isinstance(val_ty, INT) or isinstance(val_ty, BOOL)):
            return "getaddress_var", [args[0], 0, 1, 0]
        elif isinstance(expression.val, Variable):
            return "getaddress_array", [args[0], 0, 1, 0]
        else:
            return "getaddress_array", args

    elif isinstance(expression, UnopApp):
        arg1 = '%' + str(next_temorary)
        next_temorary += 1

        if isinstance(expression.arg.type, str):
                ty = scopes[0][expression.arg.type][0]
                while isinstance(ty, str):
                    ty = scopes[0][ty][0]
        else:
            ty = expression.arg.type

        op, args = evaluate_bool_expr(expression.arg, tac) if isinstance(ty, BOOL) else tmm_expressions(expression.arg, tac)
        tac["body"].append({"opcode": op, "args": args, "result": arg1})

        return operation_convert[expression.op], [arg1]

    elif isinstance(expression, BinopApp):
        arg1 = '%' + str(next_temorary)
        next_temorary += 1

        if isinstance(expression.arg_left.type, str):
                left_ty = scopes[0][expression.arg_left.type][0]
                while isinstance(left_ty, str):
                    left_ty = scopes[0][left_ty][0]
        else:
            left_ty = expression.arg_left.type

        op, args = evaluate_bool_expr(expression.arg_left, tac) if isinstance(left_ty, BOOL) else tmm_expressions(expression.arg_left, tac)
        tac["body"].append({"opcode": op, "args": args, "result": arg1})

        arg2 = '%' + str(next_temorary)
        next_temorary += 1

        op, args = evaluate_bool_expr(expression.arg_right, tac) if isinstance(left_ty, BOOL) else tmm_expressions(expression.arg_right, tac)
        tac["body"].append({"opcode": op, "args": args, "result": arg2})

        return operation_convert[expression.op], [arg1, arg2]

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

        if isinstance(instruction.type, str):
            inst_type = scopes[0][instruction.type][0]
            while isinstance(inst_type, str):
                inst_type = scopes[0][inst_type][0]
        else:
            inst_type = instruction.type

        if isinstance(inst_type, Array):
            ty = inst_type
            total_length = 1
            while isinstance(ty, Array):
                total_length *= ty.length.value
                ty = ty.type
                while isinstance(ty, str):
                    ty = scopes[0][ty][0]

            tac["body"].append(
                {'opcode': 'alloc', 'args': [total_length, ty.sizeof()], 'result': result})

        elif isinstance(inst_type, Struct):
            tac["body"].append(
                {'opcode': 'alloc', 'args': [len(inst_type.fields), max([field[1].sizeof() for field in inst_type.fields])], 'result': result})

        else:
            op, args = evaluate_bool_expr(instruction.initial, tac) if isinstance(inst_type, BOOL) else tmm_expressions(instruction.initial, tac)
            tac["body"].append(
                {"opcode": op, "args": args, "result": result})

        scopes[-1][instruction.name] = (result, instruction.lineno)

    elif isinstance(instruction, Assign):
        if isinstance(instruction.expr.type, str):
            ty_expr = scopes[0][instruction.expr.type][0]
            while isinstance(ty_expr, str):
                ty_expr = scopes[0][ty_expr][0]
        else:
            ty_expr = instruction.expr.type

        if isinstance(instruction.target, Variable):
            for scope in reversed(scopes):
                if instruction.target.name in scope:
                    result = scope[instruction.target.name][0]
                    op, args = evaluate_bool_expr(instruction.expr, tac) if isinstance(ty_expr, BOOL) else tmm_expressions(instruction.expr, tac)
                    tac["body"].append(
                        {"opcode": op, "args": args, "result": result})
                    break

        elif isinstance(instruction.target, DeRef):
            temp = '%' + str(next_temorary)
            next_temorary += 1
            op, args = tmm_expressions(instruction.target.val, tac)
            if op == 'load':
                tac['body'].append(
                    {'opcode': 'getaddress_array', 'args': args, 'result': temp})
            elif op == 'copy':
                temp = args[0]
            else:
                tac['body'].append(
                    {'opcode': op, 'args': args, 'result': temp})

            res = '%' + str(next_temorary)
            next_temorary += 1
            op, args = evaluate_bool_expr(instruction.expr, tac) if isinstance(ty_expr, BOOL) else tmm_expressions(instruction.expr, tac)
            tac["body"].append({"opcode": op, "args": args, "result": res})

            tac['body'].append({"opcode": "store", "args": [
                               res], "result": [temp, 0, 1, 0]})

        elif isinstance(instruction.target, Access):
            element = '%' + str(next_temorary)
            next_temorary += 1
            op, args = tmm_expressions(instruction.target.element, tac)
            if op == 'load':
                tac['body'].append(
                    {'opcode': 'getaddress_array', 'args': args, 'result': element})
            elif op == 'copy':
                element = args[0]
            else:
                tac['body'].append(
                    {'opcode': op, 'args': args, 'result': element})

            offset = '%' + str(next_temorary)
            next_temorary += 1

            index = '%' + str(next_temorary)
            next_temorary += 1
            op, args = tmm_expressions(instruction.target.value, tac)
            tac['body'].append({'opcode': op, 'args': args, 'result': index})

            size = '%' + str(next_temorary)
            next_temorary += 1

            if isinstance(instruction.target.type, str):
                intr_tar_ty = scopes[0][instruction.target.type][0]
                while isinstance(intr_tar_ty, str):
                    intr_tar_ty = scopes[0][intr_tar_ty][0]
            else:
                intr_tar_ty = instruction.target.type

            tac['body'].append({'opcode': 'const', 'args': [
                intr_tar_ty.sizeof()], 'result': size})

            tac['body'].append(
                {'opcode': 'mul', 'args': [index, size], 'result': offset})

            res = '%' + str(next_temorary)
            next_temorary += 1
            op, args = evaluate_bool_expr(instruction.expr, tac) if isinstance(ty_expr, BOOL) else tmm_expressions(instruction.expr, tac)
            tac["body"].append({"opcode": op, "args": args, "result": res})

            tac['body'].append({"opcode": "store", "args": [
                               res], "result": [element, 0, 1, offset]})

        elif isinstance(instruction.target, Dot):
            temp = '%' + str(next_temorary)
            next_temorary += 1
            op, args = tmm_expressions(instruction.target.element, tac)
            if op == 'load':
                tac['body'].append(
                    {'opcode': 'getaddress_array', 'args': args, 'result': temp})
            elif op == 'copy':
                temp = args[0]
            else:
                tac['body'].append(
                    {'opcode': op, 'args': args, 'result': temp})

            res = '%' + str(next_temorary)
            next_temorary += 1
            op, args = evaluate_bool_expr(instruction.expr, tac) if isinstance(ty_expr, BOOL) else tmm_expressions(instruction.expr, tac)
            tac["body"].append({"opcode": op, "args": args, "result": res})

            if isinstance(instruction.target.element.type, str):
                el_type = scopes[0][instruction.target.element.type][0]
                while isinstance(el_type, str):
                    el_type = scopes[0][el_type][0]
            else:
                el_type = instruction.target.element.type

            offset = 0
            for _, field in enumerate(el_type.fields):
                if field[0].name == instruction.target.var.name:
                    tac['body'].append({'opcode': 'store', 'args': [
                        res], 'result': [temp, 0, 1, offset]})
                    break

                if isinstance(field[1], str):
                    fl_type = scopes[0][field[1]][0]
                    while isinstance(fl_type, str):
                        fl_type = scopes[0][fl_type][0]
                else:
                    fl_type = field[1]

                offset += fl_type.sizeof()

        elif isinstance(instruction.target, To):
            temp = '%' + str(next_temorary)
            next_temorary += 1

            deref = '%' + str(next_temorary)
            next_temorary += 1

            op, args = tmm_expressions(instruction.target.element, tac)
            op_prev = op
            if op == 'load':
                tac['body'].append(
                    {'opcode': 'getaddress_array', 'args': args, 'result': deref})
                op_prev = 'getaddress_array'
            elif op == 'copy':
                deref = args[0]
            else:
                tac['body'].append({'opcode': op, 'args': args, 'result': deref})
            
            op, args = 'load', [deref, 0, 1, 0]

            if op_prev == 'getaddress_array':
                struct = '%' + str(next_temorary)
                next_temorary += 1
                tac['body'].append({'opcode': op, 'args' : args, 'result': struct})
                args = [struct, 0, 1, 0]
                op = 'load'

            if op == 'load':
                tac['body'].append(
                    {'opcode': 'getaddress_array', 'args': args, 'result': temp})
            elif op == 'copy':
                temp = args[0]
            else:
                tac['body'].append({'opcode': op, 'args': args, 'result': temp})

            res = '%' + str(next_temorary)
            next_temorary += 1
            op, args = evaluate_bool_expr(instruction.expr, tac) if isinstance(ty_expr, BOOL) else tmm_expressions(instruction.expr, tac)
            tac["body"].append({"opcode": op, "args": args, "result": res})
            
            if isinstance(instruction.target.element.type, str):
                expr_el_ty = scopes[0][instruction.target.element.type][0]
                while isinstance(expr_el_ty, str):
                        expr_el_ty = scopes[0][expr_el_ty][0]
            else:
                expr_el_ty = instruction.target.element.type
            
            if isinstance(expr_el_ty.type, str):
                root_ty = scopes[0][expr_el_ty.type][0]
                while isinstance(root_ty, str):
                    root_ty = scopes[0][root_ty][0]
            else:
                root_ty = expr_el_ty.type

            offset = 0
            for _, field in enumerate(root_ty.fields):
                if field[0].name == instruction.target.var.name:
                    tac['body'].append({'opcode': 'store', 'args': [
                        res], 'result': [temp, 0, 1, offset]})
                    break

                if isinstance(field[1], str):
                    fl_type = scopes[0][field[1]][0]
                    while isinstance(fl_type, str):
                        fl_type = scopes[0][fl_type][0]
                else:
                    fl_type = field[1]

                offset += fl_type.sizeof()

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
        if isinstance(instruction.expr.type, str):
            ty_expr = scopes[0][instruction.expr.type][0]
            while isinstance(ty_expr, str):
                ty_expr = scopes[0][ty_expr][0]
        else:
            ty_expr = instruction.expr.type

        op, args = evaluate_bool_expr(instruction.expr, tac) if isinstance(ty_expr, BOOL) else tmm_expressions(instruction.expr, tac)
        tac["body"].append(
            {"opcode": op, "args": args, "result": None})

    elif isinstance(instruction, Return):
        if isinstance(instruction.type, str):
            instr_ty = scopes[0][instruction.type][0]
            while isinstance(instr_ty, str):
                instr_ty = scopes[0][instr_ty][0]
        else:
            instr_ty = instruction.type

        if not isinstance(instr_ty, VOID):
            result = '%' + str(next_temorary)
            next_temorary += 1

            if isinstance(instruction.expr.type, str):
                ty_expr = scopes[0][instruction.expr.type][0]
                while isinstance(ty_expr, str):
                    ty_expr = scopes[0][ty_expr][0]
            else:
                ty_expr = instruction.expr.type
                
            op, args = evaluate_bool_expr(instruction.expr, tac) if isinstance(ty_expr, BOOL) else tmm_expressions(instruction.expr, tac)
            tac["body"].append(
                {"opcode": op, "args": args, "result": result})

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
        elif isinstance(instruction, TypeDecl):
            scopes[0][instruction.name] = (instruction.type, instruction.lineno)

    for instruction in instructions:
        if isinstance(instruction, list):
            for var in instruction:
                if isinstance(var.type, str):
                    var_ty = scopes[0][var.type][0]
                    while isinstance(var_ty, str):
                        var_ty = scopes[0][var_ty][0]
                else:
                    var_ty = var.type

                if isinstance(var_ty, INT):
                    tac.append(
                        {"var": "@" + var.name, "init": var.initial.value})
                elif isinstance(var_ty, BOOL):
                    tac.append(
                        {"var": "@" + var.name, "init": 0 if var.initial.value == 'false' else 1})
                elif isinstance(var_ty, Array):
                    ty = var_ty
                    total_length = 1
                    while isinstance(ty, Array):
                        total_length *= ty.length.value
                        ty = ty.type
                        while isinstance(ty, str):
                            ty = scopes[0][ty][0]

                    tac.append({"alloc": "@" + var.name, 'init': [0],  "type": None,
                               'length': total_length, 'type_size': ty.sizeof()})
                elif isinstance(var_ty, Pointer):
                    if (isinstance(var.initial.alloc_type, TypeDecl)):
                        var.initial.alloc_type = scopes[0][var.initial.alloc_type.name]
                    tac.append({"alloc": "@" + var.name, 'init': [0],  "type": None,
                               'length': var.initial.value.value, 'type_size': var.initial.alloc_type.sizeof()})

                elif isinstance(var_ty, Struct):
                    tac.append({"alloc": "@" + var.name, 'init': [0],  "type": None,
                               'length': len(var_ty.fields), 'type_size': max([field[1].sizeof() for field in var_ty.fields])})

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

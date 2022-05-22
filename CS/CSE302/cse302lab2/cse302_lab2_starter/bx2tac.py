import sys
import json
import getopt
from create_ast import *
from custom_parser import Parser

filename = ''
variables = {}
linedec = {}
next_temorary = 0
operation_covert = {"PLUS": "add", "MINUS": "sub", "TIMES": "mul", "DIV": "div", "MODULUS": "mod", "BITAND": "and", "BITOR": "or", "BITXOR": "xor",
                    "BITSHL": "shl", "BITSHR": "shr", "UMINUS": "neg", "BITCOMPL": "not"}


def tmm_expressions(expression, tac):
    global next_temorary
    global variables
    global operation_covert

    if isinstance(expression, Number):
        return "const", [expression.value]

    elif isinstance(expression, Variable):
        if expression.name not in variables:
            print(
                f'{filename}: line {expression.lineno}:Error:Use of undeclared variable "{expression.name}"')
            sys.exit(1)

        return "copy", [variables[expression.name]]

    elif isinstance(expression, UnopApp):
        arg1 = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_expressions(expression.arg, tac)
        tac[0]["body"].append({"opcode": op, "args": args, "result": arg1})

        return operation_covert[expression.op], [arg1]

    elif isinstance(expression, BinopApp):
        arg1 = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_expressions(expression.arg_left, tac)
        tac[0]["body"].append({"opcode": op, "args": args, "result": arg1})

        arg2 = '%' + str(next_temorary)
        next_temorary += 1

        op, args = tmm_expressions(expression.arg_right, tac)
        tac[0]["body"].append({"opcode": op, "args": args, "result": arg2})

        return operation_covert[expression.op], [arg1, arg2]

    else:
        print(f'Unrecognized expression type {expression}')
        raise ValueError(expression)


def tmm_statements(instructions, tac):
    global next_temorary
    global variables
    global filename

    for instruction in instructions:
        if isinstance(instruction, VarDec):
            if instruction.name in variables:
                print(
                    f'{filename}:line {instruction.lineno}:Error:Duplicate declaration of variable "{instruction.name}"')
                print(
                    f'{filename}:line {linedec[instruction.name]}:Info:Earlier declaration of "{instruction.name}"')
                sys.exit(1)

            result = '%' + str(next_temorary)
            next_temorary += 1

            op, args = tmm_expressions(instruction.initial, tac)

            linedec[instruction.name] = instruction.lineno
            variables[instruction.name] = result

            tac[0]["body"].append(
                {"opcode": op, "args": args, "result": result})

        elif isinstance(instruction, Assign):
            if instruction.target.name not in variables:
                print(
                    f'{filename}: line {instruction.target.lineno}:Error:Assigning an undeclared variable "{instruction.target.name}"')
                sys.exit(1)

            result = variables[instruction.target.name]

            op, args = tmm_expressions(instruction.expr, tac)
            tac[0]["body"].append(
                {"opcode": op, "args": args, "result": result})

        elif isinstance(instruction, Call):
            result = '%' + str(next_temorary)
            next_temorary += 1

            op, args = tmm_expressions(instruction.args[0], tac)
            tac[0]["body"].append(
                {"opcode": op, "args": args, "result": result})

            tac[0]["body"].append(
                {"opcode": instruction.function, "args": [result], "result": None})

        else:
            print(f'Unrecognized statement form: {instruction}')
            raise ValueError(instruction)


if __name__ == '__main__':
    opts, args = getopt.getopt(sys.argv[1:], '', [])
    filename = args[0]

    if not filename.endswith('.bx'):
        print(filename, 'Wrong file extention')
        raise TypeError

    with open(filename, 'r') as file:
        code = file.read()

    parser = Parser()

    try:
        instructions = parser.parse(code)
    except SyntaxError as err:
        print('Syntax error:', err)
        sys.exit(1)

    tac = [{"proc": "@main", "body": []}]

    tmm_statements(instructions, tac)

    tac_json = json.dumps(tac)
    with open(filename.split('\\')[-1].split('.')[0] + '.tac.json', 'w') as file:
        file.write(tac_json)

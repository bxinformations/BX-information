import sys
import getopt

from create_ast import *
from custom_parser import Parser


def bx2front(code, filename=""):
    main_found = False
    parser = Parser(code, filename)
    try:
        instructions = parser.parse()
        for instruction in instructions:
            if isinstance(instruction, ProcDec):
                if instruction.name in scopes[0]:
                    print(
                        f':line {instruction.lineno}:Error:Redecalred procedure "{instruction.name}" within the scope')
                    print(
                        f':line {scopes[0][instruction.name][1]}:Info:Declartion of "{instruction.name}"')
                    print(f'> {lines[instruction.lineno - 1]}')
                    print(' ' * (instruction.col + 1) + '^')
                    raise SyntaxError('Procedure redeclaration')

                if instruction.name == "main":
                    if not isinstance(instruction.type, VOID):
                        print(
                            f':line {instruction.lineno}:Error:main() procedure has to be of type "void"')
                        print(f'> {lines[instruction.lineno - 1]}')
                        print(' ' * (instruction.col + 1) + '^')
                        raise SyntaxError('Invalid return type')
                    main_found = True

                scopes[0][instruction.name] = (instruction.type, instruction.lineno, [
                                               arg.type for arg in instruction.args])

            elif isinstance(instruction, TypeDecl):
                if instruction.name in scopes[0]:
                    print(
                        f':line {instruction.lineno}:Error:Redecalred typed "{instruction.name}" within the scope')
                    print(
                        f':line {scopes[0][instruction.name][1]}:Info:Declartion of "{instruction.name}"')
                    print(f'> {lines[instruction.lineno - 1]}')
                    print(' ' * (instruction.col + 1) + '^')
                    raise SyntaxError('Type redeclaration')

                scopes[0][instruction.name] = (
                    instruction.type, instruction.lineno)
                registered_types.add(instruction.name)
        
        for instruction in instructions:
            if isinstance(instruction, list):
                for var in instruction:
                    if var.name in scopes[0]:
                        print(
                            f':line {var.lineno}:Error:Redecalred global variable "{var.name}" within the scope')
                        print(
                            f':line {scopes[0][var.name][1]}:Info:Declartion of "{var.name}"')
                        print(f'> {lines[var.lineno - 1]}')
                        print(' ' * (var.col + 1) + '^')
                        raise SyntaxError('Variable redeclaration')

                    if isinstance(var.type, str):
                        if var.type not in scopes[0]:
                            print(f'{filename}:line {var.lineno}:Error:Variable of non-existent type "{var.type}"')
                            print(f'> {lines[var.lineno - 1]}')
                            print(' ' * (var.col + 1) + '^')
                            raise SyntaxError('Invalid type')

                        var_ty = scopes[0][var.type][0]
                    else:
                        var_ty = var.type

                    if isinstance(var_ty, INT) and not isinstance(var.initial, Number):
                        print(
                            f':line {var.initial.lineno}:Error:Global variable "{var.name}" of type int decalred with a non-number value "{var.initial}"')
                        print(f'> {lines[var.initial.lineno - 1]}')
                        print(' ' * (var.initial.col + 1) + '^')
                        raise SyntaxError('Invalid declaration')

                    if isinstance(var_ty, BOOL) and not isinstance(var.initial, Bool):
                        print(
                            f':line {var.initial.lineno}:Error:Global variable "{var.name}" of type bool decalred with a non-bool value "{var.initial}"')
                        print(f'> {lines[var.initial.lineno - 1]}')
                        print(' ' * (var.initial.col + 1) + '^')
                        raise SyntaxError('Invalid declaration')

                    if isinstance(var_ty, Array) or isinstance(var_ty, Struct):
                        if not (isinstance(var.initial, Number) and var.initial.value == 0):
                            print(
                                f'{filename}:line {var.initial.lineno}:Error:Unexpected intial value of a gloabal aggegate typed variable "{var.name}"')
                            print(
                                f'{filename}:line {var.initial.lineno}:Info:0 expected')
                            print(f'> {lines[var.initial.lineno - 1]}')
                            print(' ' * (var.initial.col + 1) + '^')
                            raise SyntaxError("Invalid declaration")

                    if isinstance(var_ty, Pointer) and not isinstance(var.initial, Alloc):
                        print(
                            f':line {var.initial.lineno}:Error:Global variable "{var.name}" of type pointer decalred with a non-allocation "{var.initial}"')
                        print(f'> {lines[var.initial.lineno - 1]}')
                        print(' ' * (var.initial.col + 1) + '^')
                        raise SyntaxError('Invalid declaration')

                    elif isinstance(var_ty, Pointer) and isinstance(var.initial, Alloc) and not isinstance(var.initial.value, Number):
                        print(
                            f':line {var.initial.lineno}:Error:Global variable "{var.name}" of type pointer decalred with a non-number allocation "{var.initial}"')
                        print(f'> {lines[var.initial.lineno - 1]}')
                        print(' ' * (var.initial.col + 1) + '^')
                        raise SyntaxError('Invalid declaration')

                    scopes[0][var.name] = (var.type, var.lineno)

        if not main_found:
            print(
                f'Error:Program does not contain a main() procedure')
            raise SyntaxError('main() missing')

        for instruction in instructions:
            if isinstance(instruction, ProcDec) or isinstance(instruction, TypeDecl):
                instruction.type_check()

    except SyntaxError as err:
        print('Syntax error:', err)
        sys.exit(1)
    except ValueError as err:
        print('Value error:', err)
        sys.exit(1)
    except TypeError as err:
        print('Type error:', err)
        sys.exit(1)

    scopes.pop()
    scopes.append(dict())
    return instructions


if __name__ == '__main__':
    opts, args = getopt.getopt(sys.argv[1:], '', [])
    filename = args[0]

    if not filename.endswith('.bx'):
        print(filename, 'Wrong file extention')
        raise TypeError

    with open(filename, 'r') as file:
        code = file.read()

    print(bx2front(code, filename))
    print(scopes)

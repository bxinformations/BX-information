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
                    if instruction.type != "void":
                        print(
                            f':line {instruction.lineno}:Error:main() procedure has to be of type "void"')
                        print(f'> {lines[instruction.lineno - 1]}')
                        print(' ' * (instruction.col + 1) + '^')
                        raise SyntaxError('Invalid return type')
                    main_found = True

                scopes[0][instruction.name] = (instruction.type, instruction.lineno, [
                                               arg.type for arg in instruction.args])
            else:
                for var in instruction:
                    if var.name in scopes[0]:
                        print(
                            f':line {var.lineno}:Error:Redecalred global variable "{var.name}" within the scope')
                        print(
                            f':line {scopes[0][var.name][1]}:Info:Declartion of "{var.name}"')
                        print(f'> {lines[var.lineno - 1]}')
                        print(' ' * (var.col + 1) + '^')
                        raise SyntaxError('Variable redeclaration')

                    if var.type == 'int' and not isinstance(var.initial, Number):
                        print(
                            f':line {var.initial.lineno}:Error:Global variable "{var.name}" of type int decalred with a non-number value "{var.initial}"')
                        print(f'> {lines[var.initial.lineno - 1]}')
                        print(' ' * (var.initial.col + 1) + '^')
                        raise SyntaxError('Invalid declaration')

                    if var.type == 'bool' and not isinstance(var.initial, Bool):
                        print(
                            f':line {var.initial.lineno}:Error:Global variable "{var.name}" of type bool decalred with a non-bool value "{var.initial}"')
                        print(f'> {lines[var.initial.lineno - 1]}')
                        print(' ' * (var.initial.col + 1) + '^')
                        raise SyntaxError('Invalid declaration')

                    scopes[0][var.name] = (var.type, var.lineno)

        if not main_found:
            print(
                f'Error:Program does not contain a main() procedure')
            raise SyntaxError('main() missing')

        for instruction in instructions:
            if isinstance(instruction, ProcDec):
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

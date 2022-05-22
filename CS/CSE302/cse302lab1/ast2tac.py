import sys
from create_ast import *
import json

variables = {}
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
    
def tmm_statements(instructions, tac):
    global next_temorary
    global variables

    for instruction in instructions:
        if isinstance(instruction, VarDec):
            result = variables[instruction.name] = '%' + str(next_temorary)
            next_temorary += 1

            op, args = tmm_expressions(instruction.initial, tac)
            tac[0]["body"].append({"opcode": op, "args": args, "result": result})
        
        elif isinstance(instruction, Assign):
            result = variables[instruction.target.name]

            op, args = tmm_expressions(instruction.expr, tac)
            tac[0]["body"].append({"opcode": op, "args": args, "result": result})
        
        elif isinstance(instruction, Call):
            result = '%' + str(next_temorary)
            next_temorary += 1

            op, args = tmm_expressions(instruction.args[0], tac)
            tac[0]["body"].append({"opcode": op, "args": args, "result": result})

            tac[0]["body"].append({"opcode": instruction.function, "args": [result], "result": None})

        else:
            print(f'Unrecognized statement form: {instruction}')
            raise ValueError

def bmm_expressions(expression, tac):
    global next_temorary
    global variables
    global operation_covert

    if isinstance(expression, Number):
        result = '%' + str(next_temorary)
        next_temorary += 1

        tac[0]['body'].append({'opcode': 'const', 'args': [expression.value], 'result': result})
        return result
    
    elif isinstance(expression, Variable):
        return variables[expression.name]
    
    elif isinstance(expression, UnopApp):
        arg = bmm_expressions(expression.arg, tac)

        result = '%' + str(next_temorary)
        next_temorary += 1
        tac[0]["body"].append({"opcode": operation_covert[expression.op], "args": [arg], "result": result})

        return result
    
    elif isinstance(expression, BinopApp):
        arg_left = bmm_expressions(expression.arg_left, tac)
        arg_right = bmm_expressions(expression.arg_right, tac)

        result = '%' + str(next_temorary)
        next_temorary += 1
        tac[0]["body"].append({"opcode": operation_covert[expression.op], "args": [arg_left, arg_right], "result": result})

        return result

def bmm_statements(instructions, tac):
    global next_temorary
    global variables

    for instruction in instructions:
        if isinstance(instruction, VarDec):
            # we don't need this with given constrains, but I meant to have a more general ast to tac
            # which could do init with anythings
            arg = bmm_expressions(instruction.initial, tac)

            result = variables[instruction.name] = '%' + str(next_temorary)
            next_temorary += 1

            tac[0]["body"].append({"opcode": "copy", "args": [arg], "result": result})
        
        elif isinstance(instruction, Assign):
            arg = bmm_expressions(instruction.expr, tac)
            tac[0]["body"].append({"opcode": "copy", "args": [arg], "result": variables[instruction.target.name]})
        
        elif isinstance(instruction, Call):
            arg = bmm_expressions(instruction.args[0], tac)           
            tac[0]["body"].append({"opcode": instruction.function, "args": [arg], "result": None})

        else:
            print(f'Unrecognized statement form: {instruction}')
            raise ValueError

if __name__ == '__main__':
    mm_type = sys.argv[1]
    ast_json = sys.argv[2]

    if not ast_json.endswith('.json'):
        raise Exception

    with open(ast_json, 'r') as fp:
        js_obj = json.load(fp)

    ast = js_obj['ast'][0][0]
    procedure = ast[0][4]
    statments = procedure[0][1]

    instructions = [json_to_statment(stat) for stat in statments]
    
    tac= [{"proc" : "@main", "body" : []}]

    if mm_type == '--tmm':
        tmm_statements(instructions, tac)
    elif mm_type == '--bmm':
        bmm_statements(instructions, tac)
    else:
        print(f'Unrecognized type form: {mm_type}')
        raise ValueError

    tac_json = json.dumps(tac)
    with open(ast_json.split('.')[0] + '.tac.json', 'w') as file:
        file.write(tac_json)



    
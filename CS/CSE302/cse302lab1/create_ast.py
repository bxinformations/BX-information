class Statments:
    def __init__(self):
        return

class VarDec(Statments):
    def __init__(self, name, initial=0, type="INT"):
        self.name = name
        self.initial = initial
        self.type = type

class Assign(Statments):
    def __init__(self, target, expr):
        self.target = target
        self.expr = expr

class Call(Statments):
    def __init__(self, function, args=[]):
        # to be imporoved, here is enough when print is the only accepted function (except main)
        self.function = function
        self.args = args

class Expr:
    def __init__(self):
        return

class Variable(Expr):
    def __init__(self, name):
        self.name = name

class Number(Expr):
    def __init__(self, value):
        self.value = value

class UnopApp(Expr):
    def __init__(self, op, arg):
        self.op = op
        self.arg = arg
    
class BinopApp(Expr):
    def __init__(self, arg_left, op, arg_right):
        self.arg_left = arg_left
        self.arg_right = arg_right
        self.op = op



def json_to_expr(js_obj):
    js_obj = js_obj[0] # ignore the location component
    if js_obj[0] == '<var>':
        return Variable(js_obj[1])

    elif js_obj[0] == '<number>':
        return Number(js_obj[1])

    elif js_obj[0] == '<unop>':
        op = js_obj[1][0][0] # careful of all the nesting!
        # OP will be 'UMINUS' or 'BITCOMPL'
        arg = json_to_expr(js_obj[2]) # recursive call
        return UnopApp(op, arg)

    elif js_obj[0] == '<binop>':
        left = json_to_expr(js_obj[1])
        op = js_obj[2][0][0]
        right = json_to_expr(js_obj[3])

        return BinopApp(arg_left=left, op=op, arg_right=right)
    else:
        print(f'Unrecognized <expr> form: {js_obj}')
        raise ValueError # or whatever

def json_to_statment(js_obj):
    js_obj = js_obj[0]
    if js_obj[0] == "<vardecl>":
        name = js_obj[1][0]
        expr = json_to_expr(js_obj=js_obj[2])
        target_type = js_obj[3][0]
    
        return VarDec(name=name, initial=expr, type=target_type) 
    
    elif js_obj[0] == "<assign>":
        target = json_to_expr(js_obj[1])
        expr = json_to_expr(js_obj[2])

        return Assign(target=target, expr=expr)
    
    elif js_obj[0] == "<eval>" and js_obj[1][0][0] == "<call>":
        function = js_obj[1][0][1][0]
        args = [json_to_expr(js_obj[1][0][2][0])]

        return Call(function=function, args=args)

    else:
        print(f'Unrecognized <block> form: {js_obj}')
        raise ValueError 
import sys
import getopt
import json
from create_ast import ProcDec, VarDec


class tac_str:
    '''
        Save {"opcode": opcode, "args": args, "result": dest}
        in this class. If arg1, dest does not exists, we will let it be None
    '''

    def __init__(self, opcode, arg1, arg2, dest):
        self.opcode = opcode
        self.arg1 = arg1
        self.arg2 = arg2
        self.dest = dest

    def __str__(self):
        return (f"opcode : {self.opcode}, arg1 : {self.arg1}, arg2 : {self.arg2}, dest : {self.dest}")

    def to_tac(self):
        if (self.opcode == "load" or self.opcode == 'getaddress_var' or self.opcode == 'getaddress_array'):
            return {"opcode": self.opcode, "args": self.arg1, "result": self.dest}
        if (self.arg2 == None):
            return {"opcode": self.opcode, "args": [self.arg1], "result": self.dest}
        return {"opcode": self.opcode, "args": [self.arg1, self.arg2], "result": self.dest}

class Alloc():
    def __init__(self, name, init, type, length, type_size):
        self.name = name
        self.initial = init
        self.type = type
        self.length = length
        self.type_size = type_size
    
    def to_tac(self):
        return {"alloc": self.name, "init": self.initial, "type": self.type, "length": self.length, "type_size": self.type_size}
        

class Generator_x64:
    def __init__(self, global_var, name, args, global_arr = None):
        self.stack = {}
        self.global_var = global_var
        self.global_arr = global_arr
        self.name = name
        self.args = args
        self.strs = []
        self.stack_pos = 0
        self.tmp_var_regester = ['', '%rdi', '%rsi', '%rdx', '%rcx', '%r8', '%r9']
        self.binop = {'add': 'addq', 'sub': 'subq', 'mul': 'imulq',
                      'and': 'andq', 'or': 'orq', 'xor': 'xorq'}
        self.unop = {'not': 'notq', 'neg': 'negq'}
        self.shiftop = {'shl': 'salq', 'shr': 'sarq'}
        self.div = {'div': '%rax', 'mod': '%rdx'}
        self.condition = ['je', 'jz', 'jne', 'jnz', 'jl',
                          'jnge', 'jle', 'jng', 'jg', 'jnle', 'jge', 'jnl']

        for i in range(min(len(self.args), 6)):
            self.strs += [
                f'\tmovq {self.tmp_var_regester[i + 1]}, {self.get_pos(args[i])}']

    def get_pos(self, var, f = 1, offset = 1):
        if (isinstance(var, str) and var[:2] == '%%'):
            return var[1:]
        if (isinstance(var, int)):
            if (f):
                return f'${var}'
            else:
                return f'{var}'
        if (var in self.global_var):
            return f"{var[1:]}(%rip)"
        if (var in self.args):
            if (self.args.index(var) >= 6):
                return f"{(16 + 8 * (len(self.args) - self.args.index(var) - 1))}(%rbp)"
        if (var in self.stack.keys()):
            return f"{(-8 * self.stack[var])}(%rbp)"
        else:
            self.stack_pos += offset
            self.stack[var] = self.stack_pos - offset + 1
            return f"{(-8 * self.stack_pos)}(%rbp)"

    def to_str(self, tac_expr):
        op = tac_expr.opcode
        dest = tac_expr.dest
        arg1 = tac_expr.arg1
        arg2 = tac_expr.arg2

        if (op == 'const'):
            self.strs += [f"\tmovabsq ${arg1}, %r10",
                          f"\tmovq %r10, {self.get_pos(dest)}"]
        elif (op == 'copy'):
            if (arg1 in self.global_arr):
                self.strs += [f"\tleaq {self.get_pos(arg1)}, %r10",
                              f"\tmovq %r10, {self.get_pos(dest)}"]
            else:
                self.strs += [f"\tmovq {self.get_pos(arg1)}, %r10",
                              f"\tmovq %r10, {self.get_pos(dest)}"]
        elif op == 'label':
            self.strs += [arg1[1:] + ":"]
        elif op == 'jmp':
            self.strs += [f"\tjmp {arg1[1:]}"]
        elif op in self.binop.keys():
            op_name = self.binop[op]
            self.strs += [f"\tmovq {self.get_pos(arg1)}, %r10",
                          f"\t{op_name} {self.get_pos(arg2)}, %r10",
                          f"\tmovq %r10, {self.get_pos(dest)}"]
        elif op in self.unop.keys():
            op_name = self.unop[op]
            self.strs += [f"\tmovq {self.get_pos(arg1)}, %r10",
                          f"\t{op_name} %r10",
                          f"\tmovq %r10, {self.get_pos(dest)}"]
        elif op in self.shiftop.keys():
            op_name = self.shiftop[op]
            self.strs += [f"\tmovq {self.get_pos(arg1)}, %r10",
                          f"\tmovq {self.get_pos(arg2)}, %rcx",
                          f"\t{op_name} %cl, %r10",
                          f"\tmovq %r10, {self.get_pos(dest)}"]
        elif op in self.div.keys():
            op_name = 'idivq'
            self.strs += [f"\tmovq {self.get_pos(arg1)}, %rax",
                          f"\tcqto",
                          f"\t{op_name} {self.get_pos(arg2)}",
                          f"\tmovq {self.div[op]}, {self.get_pos(dest)}"]
        elif op in self.condition:
            self.strs += [f"\tmovq {self.get_pos(arg1)},%r10",
                          f"\tcmpq $0, %r10",
                          f"\t{op} {arg2[1:]}"]
        elif op == 'param':
            if arg1 <= 6:
                self.strs += [
                    f"\tmovq {self.get_pos(arg2)} , {self.tmp_var_regester[arg1]}"]
            else:
                self.strs += [f"\tpushq {self.get_pos(arg2)}"]
        elif op == 'call':
            self.strs += [f'\tcallq {arg1[1:]}']
            if dest != None:
                self.strs += [f'\tmovq %rax, {self.get_pos(dest)}']
        elif op == 'ret':
            if arg1 == None:
                self.strs += ['\txorq %rax, %rax',
                              f'\tjmp .Lend_{self.name[1:]}']
            else:
                self.strs += [f'\tmovq {self.get_pos(arg1)}, %rax',
                              f'\tjmp .Lend_{self.name[1:]}']
        elif op == 'load':
            if (arg1[0] in self.global_var):
                self.strs += [f'\tleaq {self.get_pos(arg1[0])}, %r12']
            else:
                self.strs += [f'\tmovq {self.get_pos(arg1[0])}, %r12']
            self.strs += [f'\tmovq {self.get_pos(arg1[1])}, %r13',
                          f'\tmovq {self.get_pos(arg1[3])}, %r14',
                          f'\taddq %r14, %r13',
                          f'\tleaq 0(%r12, %r13, {self.get_pos(arg1[2], 0)}), %r10',
                          f'\tmovq (%r10), %r11',
                          f'\tmovq %r11, {self.get_pos(dest)}']
        elif op == 'getaddress_var':
            self.strs += [f'\tleaq {self.get_pos(arg1[0])}, %r12',
                          f'\tmovq {self.get_pos(arg1[1])}, %r13',
                          f'\tmovq {self.get_pos(arg1[3])}, %r14',
                          f'\taddq %r14, %r13',
                          f'\tleaq 0(%r12, %r13, {self.get_pos(arg1[2], 0)}), %r10',
                          f'\tmovq %r10, {self.get_pos(dest)}']
        elif op == 'getaddress_array':
            if (arg1[0] in self.global_var):
                self.strs += [f'\tleaq {self.get_pos(arg1[0])}, %r12']
            else:
                self.strs += [f'\tmovq {self.get_pos(arg1[0])}, %r12']
            self.strs += [f'\tmovq {self.get_pos(arg1[1])}, %r13',
                          f'\tmovq {self.get_pos(arg1[3])}, %r14',
                          f'\taddq %r14, %r13',
                          f'\tleaq 0(%r12, %r13, {self.get_pos(arg1[2], 0)}), %r10',
                          f'\tmovq %r10, {self.get_pos(dest)}']
        elif op == 'store':
            if (dest[0] in self.global_var):
                self.strs += [f'\tleaq {self.get_pos(dest[0])}, %r12']
            else:
                self.strs += [f'\tmovq {self.get_pos(dest[0])}, %r12']
            self.strs += [f'\tmovq {self.get_pos(dest[1])}, %r13',
                          f'\tmovq {self.get_pos(dest[3])}, %r14',
                          f'\taddq %r14, %r13',
                          f'\tleaq 0(%r12, %r13, {self.get_pos(dest[2], 0)}), %r10',
                          f'\tmovq {self.get_pos(arg1)}, %r11',
                          f'\tmovq %r11, (%r10)']
        elif op == 'alloc':
            self.strs += [f'\tmovq {self.get_pos(arg1)}, %rdi',
                          f'\tmovq {self.get_pos(arg2)}, %rsi',
                          f'\tcallq calloc',
                          f'\tmovq %rax, {self.get_pos(dest)}']
        else:
            ValueError("Unknown Opcode")

    def main(self, tac_file):
        for line in tac_file:
            self.to_str(line)
        head = [f'\t.globl {self.name[1:]}', '\t.text', f'{self.name[1:]}:', '\tpushq %rbp',
                '\tmovq %rsp, %rbp', f'\tsubq ${8 * len(self.stack)}, %rsp']
        if (self.name[1:] == 'main'):
            tail = [f'.Lend_{self.name[1:]}:', '\tmovq %rbp, %rsp ', '\tpopq %rbp ',
                    '\txorq %rax, %rax', '\tretq', '']
        else:
            tail = [f'.Lend_{self.name[1:]}:', '\tmovq %rbp, %rsp ', '\tpopq %rbp ',
                    '\tretq', '']
        return head + self.strs + tail


def load_tac(js_obj):
    var = []
    proc = []
    for now in js_obj:
        if "proc" in now.keys():
            # print(now["proc"])
            tac = []
            for line in now["body"]:
                op = line["opcode"]
                if (op == "load" or op == 'getaddress_var' or op == 'getaddress_array'):
                    arg1 = line["args"]
                    arg2 = None
                    if (arg1[0] == None):
                        arg1[0] = 0
                    if (arg1[1] == None):
                        arg1[1] = 0
                    if (arg1[2] == None):
                        arg1[2] = 1
                    if (arg1[3] == None):
                        arg1[3] = 0
                    dest = line["result"]
                    tac.append(tac_str(op, arg1, arg2, dest))
                    continue
                if (op == "store"):
                    arg1 = line["args"][0]
                    dest = line["result"]
                    arg2 = None
                    if (dest[0] == None):
                        dest[0] = 0
                    if (dest[1] == None):
                        dest[1] = 0
                    if (dest[2] == None):
                        dest[2] = 1
                    if (dest[3] == None):
                        dest[3] = 0
                    tac.append(tac_str(op, arg1, arg2, dest))
                    continue
                arg1 = None
                arg2 = None
                dest = line["result"]
                if (len(line["args"]) == 1):
                    arg1 = line["args"][0]
                    if (isinstance(arg1, str)) and (arg1[:3] == '%.L'):
                        arg1 = arg1 + "_" + now["proc"][1:]
                elif (len(line["args"]) == 2):
                    arg1 = line["args"][0]
                    arg2 = line["args"][1]
                    if (isinstance(arg1, str)) and (arg1[:3] == '%.L'):
                        arg1 = arg1 + "_" + now["proc"][1:]
                    if (isinstance(arg2, str)) and (arg2[:3] == '%.L'):
                        arg2 = arg2 + "_" + now["proc"][1:]
                tac.append(tac_str(op, arg1, arg2, dest))
            proc.append(ProcDec(now["proc"], now["args"], None, tac, None, None))
        elif "var" in now:
            var.append(VarDec(now["var"], now["init"], None, None, None))
        elif "alloc" in now:
            var.append(Alloc(now["alloc"], now["init"], now["type"], now["length"], now["type_size"]))
    return var, proc


def main(var, proc):
    result = []
    global_var = []
    global_arr = []
    for i in var:
        global_var.append(i.name)
        if (isinstance(i, Alloc)):
            global_arr.append(i.name)
            result = result + \
                [f'\t.globl {i.name[1:]}', '\t.data',
                 f'{i.name[1:]}:']
            for j in i.initial:
                result += [f'\t.quad {j}']
            length = i.length
            type_size = i.type_size
            if (type_size > 8):
                total = length * type_size
                length = int(total / 8)
                type_size = 8
            init = f'\t.fill {length}, {type_size}, {0}'
            result += [init]
            result += ['']
        else:
            result = result + \
                [f'\t.globl {i.name[1:]}', '\t.data',
                    f'{i.name[1:]}:  .quad {i.initial}', '']
    # print("--------------")
    # print(result)
    # print("---------------")
    for i in proc:
        now = Generator_x64(global_var, i.name, i.args, global_arr)
        # print("---------------")
        # print(i.name)
        now_x86 = now.main(i.body)
        result += now_x86
        # for i in now_x86:
        # print(i)
        # print('-----------')
    return result


def tac2x64(file_name):
    with open(file_name, 'r') as fp:
        js_obj = json.load(fp)
        var, proc = load_tac(js_obj)
    out = main(var, proc)
    return out


if __name__ == '__main__':
    opts, args = getopt.getopt(sys.argv[1:], '', [])
    tac = []
    with open(args[0], 'r') as fp:
        js_obj = json.load(fp)
        var, proc = load_tac(js_obj)

    out = main(var, proc)
    f_out = open(args[0][:-5] + '.s', 'w')
    for i in out:
        f_out.write(i + '\n')
    f_out.close()
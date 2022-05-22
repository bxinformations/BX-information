import json
import sys
import getopt


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


class Generator_x64:
    def __init__(self):
        self.stack = {}
        self.strs = []
        self.stack_pos = 0
        self.binop = {'add': 'addq', 'sub': 'subq', 'mul': 'imulq',
                      'and': 'andq', 'or': 'orq', 'xor': 'xorq'}
        self.unop = {'not': 'notq', 'neg': 'negq'}
        self.shiftop = {'shl': 'salq', 'shr': 'sarq'}
        self.div = {'div': '%rax', 'mod': '%rdx'}
        self.condition = ['je', 'jz', 'jne', 'jnz', 'jl',
                          'jnge', 'jle', 'jng', 'jg', 'jnle', 'jge', 'jnl']

    def get_pos(self, var):
        if (var in self.stack.keys()):
            return self.stack[var]
        else:
            self.stack_pos += 1
            self.stack[var] = self.stack_pos
            return self.stack_pos

    def to_str(self, tac_expr):
        op = tac_expr.opcode
        dest = tac_expr.dest
        arg1 = tac_expr.arg1
        arg2 = tac_expr.arg2

        if (op == 'const'):
            pos = self.get_pos(dest)
            self.strs += [f"\tmovabsq ${arg1}, %rax",
                          f"\tmovq %rax, {-8 * pos}(%rbp)"]
        elif (op == 'copy'):
            pos = self.get_pos(dest)
            self.strs += [f"\tmovq {-8 * self.get_pos(arg1)}(%rbp), %r8",
                          f"\tmovq %r8, {-8 * pos}(%rbp)"]
        elif (op == 'print'):
            self.strs += [
                f"\tmovq {-8 * self.get_pos(arg1)}(%rbp), %rdi", "\tcallq bx_print_int"]
        elif op == 'label':
            self.strs += [arg1[1:] + ":"]
        elif op == 'jmp':
            self.strs += [f"\tjmp {arg1[1:]}"]
        elif op in self.binop.keys():
            op_name = self.binop[op]
            self.strs += [f"\tmovq {-8 * self.get_pos(arg1)}(%rbp), %r8",
                          f"\t{op_name} {-8 * self.get_pos(arg2)}(%rbp), %r8",
                          f"\tmovq %r8, {-8 * self.get_pos(dest)}(%rbp)"]
        elif op in self.unop.keys():
            op_name = self.unop[op]
            self.strs += [f"\tmovq {-8 * self.get_pos(arg1)}(%rbp), %r8",
                          f"\t{op_name} %r8",
                          f"\tmovq %r8, {-8 * self.get_pos(dest)}(%rbp)"]
        elif op in self.shiftop.keys():
            op_name = self.shiftop[op]
            self.strs += [f"\tmovq {-8 * self.get_pos(arg1)}(%rbp), %r8",
                          f"\tmovq {-8 * self.get_pos(arg2)}(%rbp), %rcx",
                          f"\t{op_name} %cl, %r8",
                          f"\tmovq %r8, {-8 * self.get_pos(dest)}(%rbp)"]
        elif op in self.div.keys():
            op_name = 'idivq'
            self.strs += [f"\tmovq {-8 * self.get_pos(arg1)}(%rbp), %rax",
                          f"\tcqto",
                          f"\t{op_name} {-8 * self.get_pos(arg2)}(%rbp)",
                          f"\tmovq {self.div[op]}, {-8 * self.get_pos(dest)}(%rbp)"]
        elif op in self.condition:
            self.strs += [f"\tmovq {-8 * self.get_pos(arg1)}(%rbp),%rax",
                          f"\tcmpq $0, %rax",
                          f"\t{op} {arg2[1:]}"]
        else:
            ValueError("Unknown Opcode")

    def main(self, tac_file):
        for line in tac_file:
            self.to_str(line)
        head = ['\t.globl main', '\t.text', 'main:', '\tpushq %rbp',
                '\tmovq %rsp, %rbp', f'\tsubq ${8 * len(self.stack)}, %rsp']
        tail = ['\tmovq %rbp, %rsp ', '\tpopq %rbp ',
                '\tmovq $0, %rax', '\tretq']
        return head + self.strs + tail


def load_tac(js_obj):
    tac = []
    for line in js_obj[0]["body"]:
        op = line["opcode"]
        arg1 = None
        arg2 = None
        dest = line["result"]
        if (len(line["args"]) == 1):
            arg1 = line["args"][0]
        elif (len(line["args"]) == 2):
            arg1 = line["args"][0]
            arg2 = line["args"][1]
        tac.append(tac_str(op, arg1, arg2, dest))
    return tac


def tac2x64(file_name):
    tac = []
    with open(file_name, 'r') as fp:
        js_obj = json.load(fp)
        tac = load_tac(js_obj)

    x86_file = Generator_x64()
    out = x86_file.main(tac)
    return out


if __name__ == '__main__':
    opts, args = getopt.getopt(sys.argv[1:], '', [])
    tac = []
    with open(args[0], 'r') as fp:
        js_obj = json.load(fp)
        tac = load_tac(js_obj)

    x86_file = Generator_x64()
    out = x86_file.main(tac)
    f_out = open(args[0][:-5] + '.s', 'w')
    for i in out:
        f_out.write(i + '\n')
    f_out.close()

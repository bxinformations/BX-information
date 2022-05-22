import sys
import getopt
import json
from create_ast import ProcDec, VarDec
from tac2x64 import tac_str


class Instructions:
    def __init__(self, instruction):
        self.instruction = instruction
        self.id = id(instruction)
        self.child = []

    def check(self, tmp):
        return (tmp != None) and (isinstance(tmp, str)) and (tmp[0] == '%')

    def __str__(self):
        return f'opcode : {self.instruction.opcode}, arg : {[self.instruction.arg1, self.instruction.arg2]}, dest : {self.instruction.dest}'


class Block:
    def __init__(self, instructions):
        self.label = instructions[0].instruction.arg1
        self.instructions = instructions
        self.child = []
        self.father = []

    def add_father(self, father):
        self.father.append(father)

    def add_child(self, child):
        self.child.append(child)

    def union(self, tmp):
        self.instructions += tmp.instructions
        for child_label in tmp.child:
            self.child.append(child_label)


class CFG:
    def __init__(self, tac_file, proc_name):
        self.name = proc_name[1:]
        self.build(tac_file)

    def build(self, tac_file):
        # print("------------build------------------")
        self.block = {}
        self.instrcutions = {}
        self.tac_id = []

        # add entry label
        if (tac_file[0].opcode != 'label'):
            tmp = ".Lentry_" + self.name
            tac_file.insert(0, tac_str('label', tmp, None, None))
            self.entry_label = tmp
        else:
            self.entry_label = tac_file[0].arg1

        cnt = 0

        i = 0
        while (i < len(tac_file)):
            now = tac_file[i]
            if (i < len(tac_file) - 1):
                next = tac_file[i + 1]
            if (now.opcode == 'jmp') and (i < len(tac_file) - 1) and (next.opcode != 'label'):
                tmp = tac_str('label', f'.Ljmp_{self.name}_{cnt}', None, None)
                cnt += 1
                tac_file.insert(i + 1, tmp)

            elif (i >= 1) and (now.opcode == 'label') and (tac_file[i - 1].opcode != 'jmp'):
                if (tac_file[i - 1].opcode == 'ret'):
                    tac_file.insert(i, tac_str('jmp', now.arg1, None, 'tmp'))
                else:
                    tac_file.insert(i, tac_str('jmp', now.arg1, None, None))

            tmp = Instructions(tac_file[i])
            self.instrcutions[tmp.id] = tmp
            self.tac_id.append(tmp.id)
            i += 1

        self.block[tac_file[0].arg1] = Block(
            [self.instrcutions[self.tac_id[0]]])
        prev = tac_file[0].arg1

        edge = []
        con_jmp = {'jz', 'jnz', 'jl', 'jle', 'jnl', 'jnle'}

        for i in range(1, len(tac_file)):
            now = tac_file[i]
            id_now = self.tac_id[i]
            if (now.opcode == 'label'):
                self.block[now.arg1] = Block([self.instrcutions[id_now]])
                prev = now.arg1
            else:
                self.block[prev].instructions.append(self.instrcutions[id_now])

            if now.opcode in con_jmp:
                next = now.arg2
                edge.append((prev, next))
            if now.opcode == 'jmp':
                next = now.arg1
                edge.append((prev, next))

        # for i in (self.block.keys()):
            # print(i)

        for i in range(1, len(tac_file)):
            now = tac_file[i]
            now_id = self.tac_id[i]
            now_op = now.opcode
            if (now_op == 'label'):
                continue

            if now_op in con_jmp:
                next = now.arg2
                if (len(self.block[next].instructions) > 1):
                    self.instrcutions[now_id].child.append(
                        self.block[next].instructions[1].id)
            if now_op == 'jmp':
                next = now.arg1
                if (len(self.block[next].instructions) > 1):
                    self.instrcutions[now_id].child.append(
                        self.block[next].instructions[1].id)
                continue

            if (i < len(tac_file) - 1):
                next = self.instrcutions[self.tac_id[i + 1]].instruction
                if (next.opcode != 'label'):
                    self.instrcutions[now_id].child.append(self.tac_id[i + 1])

        for (father, child) in edge:
            self.block[father].add_child(child)
            self.block[child].add_father(father)

        # for i in self.instrcutions.values():
            # print(i)

    def jump_thread(self):

        con_jmp = {'jz', 'jnz', 'jl', 'jle', 'jnl', 'jnle'}

        def rec_linearlizasion(now, visted):
            if (len(now.child) == 1):
                child_label = list(now.child)[0]
                block = self.block[child_label]
                if (len(block.father) == 1):
                    visted += [child_label]
                    return rec_linearlizasion(block, visted)
            return visted

        noNeedBlock = set()

        for label, block in self.block.items():
            linearlize = rec_linearlizasion(block, [label])[:-1]

            if (len(linearlize) > 1):
                f = True

                for i in range(1, len(linearlize) - 1):
                    if (len(linearlize[i]) > 2):
                        f = False
                        break
                if f:
                    head = self.block[linearlize[0]]
                    head.instructions[-1].instruction.arg1 = linearlize[-1]
                    for i in linearlize[1:-1]:
                        head.union(self.block[i])
                        noNeedBlock.add(i)

            for child_label in block.child:
                con_variable = None  # which variable be checked if jumps to child or not
                con_jmp_used = None  # which kinds of condition jumps
                for instr in block.instructions:
                    if (instr.instruction.arg2 == child_label):
                        if instr.instruction.opcode in con_jmp:
                            con_variable = instr.instruction.arg1
                            con_jmp_used = instr.instruction.opcode

                if not con_variable:
                    continue

                f = False
                child = self.block[child_label]
                for instr in child.instructions:
                    if instr.instruction.dest == con_variable:
                        f = True
                        break
                if f:
                    continue

                f = False
                for i in range(len(child.instructions) - 1):
                    now = child.instructions[i].instruction
                    if (len(child.father) != 1):
                        break
                    if (now.opcode == con_jmp_used) and (now.arg1 == con_variable):
                        next_label = now.arg2
                        child.instructions[i].instruction.opcode = 'nop'
                        if child.instructions[i + 1].instruction.opcode == 'jmp':
                            child.instructions[i +
                                               1].instruction.arg1 = next_label
                        f = True

        for i in noNeedBlock:
            self.block.pop(i, None)
            self.remove(i)

    def cleaned(self, flag=True):
        entry = self.block[self.entry_label]
        visited_str = list(entry.instructions)
        visited_label = set([self.entry_label])

        def rec_linearlizasion(now):
            if (now.instructions[-1].instruction.opcode == 'jmp'):
                if (now.instructions[-1].instruction.arg1 not in visited_label):
                    child = self.block[now.instructions[-1].instruction.arg1]
                    visited_label.add(now.instructions[-1].instruction.arg1)
                    if (child.instructions[-1].instruction.opcode != 'jmp') and \
                        (child.instructions[-1].instruction.opcode != 'ret'):
                        child.instructions += [Instructions(
                            tac_str('ret', None, None, None))]
                    visited_str.extend(child.instructions)
                    rec_linearlizasion(child)
            for child_label in now.child:
                if child_label in visited_label:
                    continue
                child = self.block[child_label]
                visited_label.add(child_label)
                if (child.instructions[-1].instruction.opcode != 'jmp') and \
                    (child.instructions[-1].instruction.opcode != 'ret'):
                    child.instructions += [Instructions(
                        tac_str('ret', None, None, None))]
                visited_str.extend(child.instructions)
                rec_linearlizasion(child)

        rec_linearlizasion(entry)

        con_jmp = {'jz', 'jnz', 'jl', 'jle', 'jnl', 'jnle'}

        # print("--------- cleaning dead code")
        # for i in visited_str:
        # print((i.instruction))
        # print("------------end")

        # if flag:
        # for i in visited_str:
        # print(i)

        for str in visited_str:
            if (str.instruction.opcode == 'jmp') and (str.instruction.dest == 'tmp'):
                str.instruction.opcode = 'nop'
                continue
            if str.instruction.opcode != 'label':
                continue
            label = str.instruction.arg1
            f = False
            for i in visited_str:
                if i.instruction.opcode in con_jmp:
                    if (label == i.instruction.arg2):
                        f = True
                        break
                elif i.instruction.opcode == 'jmp':
                    if label == i.instruction.arg1:
                        f = True
                        break
            if not f:
                str.instruction.opcode = 'nop'

        if flag:
            tac = []
            for i in visited_str:
                # print(i)
                tac.append(i.instruction)
            cnt = 0
            for i in range(len(tac) - 1):
                j = i - cnt
                now = tac[j]
                if now.opcode == 'jmp':
                    next = tac[j + 1]
                    if (next.opcode == 'label') and (next.arg1 == now.arg1):
                        # print(j)
                        # print(now)
                        # print(next)
                        tac.pop(j)
                        cnt += 1
            cnt = 0
            for i in range(len(tac) - 1):
                j = i - cnt
                now = tac[j]
                if now.opcode == 'ret':
                    if now.arg1 == None:
                        next = tac[j + 1]
                        if (next.opcode == 'ret'):
                            tac.pop(j)
                            cnt += 1
                    else:
                        next = tac[j + 1]
                        if (next.opcode == 'ret'):
                            tac.pop(j + 1)
                            cnt += 1
        else:
            tac = [i.instruction for i in visited_str]

        # if flag:
            # print('---------------cleaned dead code--------')
            # for i in tac:
            # print(i)
        return list(filter(lambda instr: instr.opcode != 'nop', tac))

    def clean_dead_code(self):
        new_str = self.cleaned(False)
 #       for i in new_str:
 #           print(str(i))
        self.build(new_str)

    def remove(self, block_label):
        for block in self.block.values():
            if block_label in block.child:
                block.child.remove(block_label)
            if block_label in block.father:
                block.father.remove(block_label)

    def coaleasce_blocks(self):
        for block in self.block.values():
            if (len(block.child) == 1):
                child_label = list(block.child)[0]
                child = self.block[child_label]
                if (len(child.father) != 1):
                    continue
                if (child_label == self.entry_label):
                    continue
                if (block.instructions[-1].instruction.opcode == 'jmp'):
                    block.instructions[-1].instruction.opcode = 'nop'
                    block.union(child)
                    self.block.pop(child_label)
                    self.remove(child_label)
                    return (block.label, child_label)
        return False

    def coaleasce(self):
        coalesced = set()
        while 1:
            now = self.coaleasce_blocks()
            self.clean_dead_code()
            if (now) and (now not in coalesced):
                coalesced.add(now)
            else:
                break

    def optimize(self):
        self.jump_thread()
        self.clean_dead_code()
        self.coaleasce()
        # print('--------------end optimize------------')
        # for i in self.instrcutions.values():
        # print(i)


def load_tac(js_obj):
    result = []
    for now in js_obj:
        if "proc" in now.keys():
            tac = []
            for line in now["body"]:
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
            result.append(
                ProcDec(now["proc"], now["args"], None, tac, None, None))
        elif "var" in now:
            result.append(VarDec(now["var"], now["init"], None, None, None))
    return result


def tac_cf(filename):
    gvars = []
    procs = []
    with open(filename, 'r') as fp:
        js_obj = json.load(fp)
        tac = load_tac(js_obj)
    for decl in tac:
        if isinstance(decl, ProcDec):
            if decl.body == []:
                procs.append(ProcDec(decl.name, decl.args,
                             None, decl.body, None, None))
                continue
            cfg = CFG(decl.body, decl.name)
            cfg.optimize()
            proc_instrs = cfg.cleaned()
            procs.append(ProcDec(decl.name, decl.args,
                         None, proc_instrs, None, None))
        elif isinstance(decl, VarDec):
            gvars.append(decl)

    with open(f'{filename[:-4]}tac_optimized.json', 'w') as tac_file:
        tac = []
        for i in gvars + procs:
            tac.append(i.to_tac())
        out = json.dumps(tac)
        tac_file.write(out)


if __name__ == "__main__":
    opts, args = getopt.getopt(sys.argv[1:], '', [])
    tac_cf(args[0])

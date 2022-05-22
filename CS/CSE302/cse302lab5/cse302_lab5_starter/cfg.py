#!/usr/bin/env python3

"""
Control Flow Graphs (CFG)
"""

import re
import tac
from io import StringIO

# ------------------------------------------------------------------------------


class Block:
    """Basic block -- does not support multiple labels"""

    def __init__(self, label, body=None, jumps=None):
        self.label = label
        self.body = list(body or [])
        self.jumps = list(jumps or [])

    def instrs(self):
        """Iterator over the instructions in the block (excluding label)"""
        for instr in self.body:
            yield instr
        for instr in self.jumps:
            yield instr

    def reversed_instrs(self):
        """Reversed iterator over the instructions in the block (excluding label)"""
        for instr in reversed(self.jumps):
            yield instr
        for instr in reversed(self.body):
            yield instr

    def first_instr(self):
        return next(self.instrs())

    def last_instr(self):
        return next(self.reversed_instrs())

    def instr_pairs(self):
        """Iterator over all 2-instruction sequences wholly inside the current block"""
        iseq = self.reversed_instrs()
        last = next(iseq)
        for cur in iseq:
            yield (cur, last)
            last = cur

    def display(self, file=None):
        print(f'{self.label}:', file=file)
        for instr in self.instrs():
            print(f'{instr}', file=file)

    def __str__(self):
        s = StringIO()
        self.display(file=s)
        return s.getvalue()

# ------------------------------------------------------------------------------


def get_jump_dest(jinstr):
    if jinstr.opcode == 'jmp':
        return jinstr.arg1
    if jinstr.opcode != 'ret':
        return jinstr.arg2
    # return None otherwise


class CFG:
    """Control flow graph"""

    def __init__(self, proc_name, lab_entry, blocks):
        """
        proc_name: name of the proc
        lab_entry: label of the entry block
        blocks: list of Block
          (blocks will be identified by their labels)
        """
        self.proc_name = proc_name
        self.lab_entry = lab_entry
        self._blockmap = {bl.label: bl for bl in blocks}
        self._fwd = {lab: set() for lab in self._blockmap}  # next()
        self._bwd = {lab: set() for lab in self._blockmap}  # prev()
        # now link up the blocks
        for bl in blocks:
            for jinstr in bl.jumps:
                dest = get_jump_dest(jinstr)
                if dest:
                    self.add_edge(bl.label, dest)

    def __getitem__(self, lab):
        """
        Return the block with the input label `lab'.
        Recall that cfg.__getitem__(lab) can be written as cfg[lab].
        """
        return self._blockmap[lab]

    def successors(self, lab):
        """Returns iterator over immediate successor blocks"""
        return iter(self._fwd[lab])

    def out_degree(self, lab):
        return len(self._fwd[lab])

    def predecessors(self, lab):
        """Returns iterator over immediate predecessor blocks"""
        return iter(self._bwd[lab])

    def in_degree(self, lab):
        return len(self._bwd[lab])

    def nodes(self):
        return iter(self._blockmap.values())

    def edges(self):
        """
        Returns an iterator over all the edges.
        Each edge is represented as 2-tuples of (source, target) labels.
        """
        for lab_from in self._fwd:
            for lab_to in self._fwd[lab_from]:
                yield (lab_from, lab_to)

    def add_node(self, block):
        assert block.label not in self._blockmap
        self._blockmap[block.label] = block
        self._fwd[block.label] = set()
        self._bwd[block.label] = set()
        for jinstr in block.jumps:
            dest = get_jump_dest(jinstr)
            if dest:
                self.add_edge(block.label, dest)

    def remove_node(self, block):
        assert block.label in self._blockmap
        del self._blockmap[block.label]
        for lab_to in self._fwd[block.label]:
            self._bwd[lab_to].remove(block.label)
        del self._fwd[block.label]

    def add_edge(self, lab_from, lab_to):
        self._fwd[lab_from].add(lab_to)
        self._bwd[lab_to].add(lab_from)

    def remove_edge(self, lab_from, lab_to):
        self._fwd[lab_from].remove(lab_to)
        self._bwd[lab_to].remove(lab_from)

    def instrs(self):
        for bl in self._blockmap.values():
            yield from bl.instrs()

    def instr_pairs(self, labeled=False):
        """
        The order of visiting the sequences is unspecified. If `labeled' is
        True, then yield a 4-tuple of the form (l1, i1, l2, i2) where
        l1 and l2 are the labels of the blocks containing i1 and i2.
        """
        # iterate over the edges
        for lab_from, lab_to in self.edges():
            i1 = self._blockmap[lab_from].last_instr()
            i2 = self._blockmap[lab_to].first_instr()
            if labeled:
                yield (lab_from, i1, lab_to, i2)
            else:
                yield (i1, i2)
        # iterate over the instruction pairs inside a block
        for bl in self._blockmap.values():
            if labeled:
                for i1, i2 in bl.instr_pairs():
                    yield (bl.label, i1, bl.label, i2)
            else:
                yield from bl.instr_pairs()

    def write_dot(self, tacfile, **kwargs):
        dotfile = f'{tacfile}.{self.proc_name[1:]}.dot'
        with open(dotfile, 'w') as f:
            print(f'digraph {self.proc_name[1:]} {{', file=f)
            for bl in self._blockmap.values():
                node_text = str(bl).replace('\n', r'\l')
                first_break = node_text.find(r'\l') + 2
                node_label, node_text = node_text[:
                                                  first_break], node_text[first_break:]
                if 'livein' in kwargs:
                    node_label += '  // LI: {' + \
                        ','.join(kwargs['livein'][bl.first_instr()]) + r'}\l'
                if 'liveout' in kwargs:
                    node_text += '  // LO: {' + \
                        ','.join(kwargs['liveout'][bl.last_instr()]) + r'}\l'
                node_text = node_label + node_text
                print(
                    f'{bl.label[2:]}[shape="box",fontname="monospace",fontsize=8,label="{node_text}"];', file=f)
            for lab_from, lab_tos in self._fwd.items():
                for lab_to in lab_tos:
                    print(f'{lab_from[2:]} -> {lab_to[2:]};', file=f)
            print('}', file=f)

# ------------------------------------------------------------------------------


_enders = re.compile(r'jmp|jz|jnz|jl|jle|jnl|jnle|ret')
_jumps = re.compile(r'jmp|jz|jnz|jl|jle|jnl|jnle')
_jcc = re.compile(r'jz|jnz|jl|jle|jnl|jnle')
_jabs = re.compile(r'jmp|ret')
_unconditional = re.compile(r'label|jmp|ret')


def apply_label_rewrite(jinstr, tab):
    if jinstr.opcode == 'jmp':
        jinstr.arg1 = tab.get(jinstr.arg1, jinstr.arg1)
    elif jinstr.opcode == 'phi':
        jinstr.arg1 = tuple((tab.get(lab, lab), tmp)
                            for (lab, tmp) in jinstr.arg1.items())
    elif jinstr.opcode != 'ret':
        jinstr.arg2 = tab.get(jinstr.arg2, jinstr.arg2)


class counter:
    """A simple counter"""

    def __init__(self, *, start=None, transfn=None):
        self._transfn = transfn
        self._count = start or 0

    def __iter__(self):
        return self

    def __next__(self):
        c = self._count
        self._count += 1
        if self._transfn:
            return self._transfn(c)
        return c


def normalize_labels(tac_proc):
    """Cleanup `tac_proc' to remove multiple entry labels, and renumber as
    %.L0, %.L1, ..."""
    labels = counter(transfn=lambda n: f'%.L{n}')
    norm_map = dict()
    instrs, tac_proc.body = tac_proc.body, []
    cur = 0
    while cur < len(instrs):
        instr = instrs[cur]
        cur += 1
        if instr.opcode == 'label':
            assert instr.arg1 not in norm_map
            lab = next(labels)
            tac_proc.body.append(tac.Instr(None, 'label', (lab, None)))
            norm_map[instr.arg1] = lab
            while cur < len(instrs) and instrs[cur].opcode == 'label':
                norm_map[instrs[cur].arg1] = lab
                cur += 1
        else:
            tac_proc.body.append(instr)
    for instr in tac_proc.body:
        apply_label_rewrite(instr, norm_map)


def fallthrough_to_jump(tac_proc):
    """Replace all fallthroughs to explicit jumps.
    Assumes that `tac_proc' always has a label after a jump."""
    instrs, tac_proc.body = tac_proc.body, []
    for cur, instr in enumerate(instrs):
        tac_proc.body.append(instr)
        if (not _unconditional.fullmatch(instr.opcode) and
            cur + 1 < len(instrs) and
                instrs[cur + 1].opcode == 'label'):
            tac_proc.body.append(
                tac.Instr(None, 'jmp', (instrs[cur + 1].arg1, None)))


def add_admin_labels(tac_proc):
    """Add labels everywhere they may be needed for basic blocks inference.
    At the end, every jump will be followed by at least one label"""
    instrs, tac_proc.body = tac_proc.body, []
    name = tac_proc.name[1:]
    admin_labels = counter(transfn=lambda x: f'%.L{name}{x}')
    tac_proc.body.append(tac.Instr(None, 'label', (next(admin_labels), None)))
    cur = 0
    while cur < len(instrs):
        instr = instrs[cur]
        tac_proc.body.append(instr)
        cur += 1
        if _jcc.fullmatch(instr.opcode):
            # skip conditional jump sequences
            while cur < len(instrs):
                instr = instrs[cur]
                if not _jcc.fullmatch(instr.opcode):
                    break
                tac_proc.body.append(instr)
                cur += 1
            # skip unconditional jump
            instr = instrs[cur]
            if _jabs.fullmatch(instr.opcode):
                tac_proc.body.append(instr)
                cur += 1
            tac_proc.body.append(
                tac.Instr(None, 'label', (next(admin_labels), None)))


def infer(tac_proc):
    """Return a CFG inferred from the proc"""
    # First bring it into a canonical form
    add_admin_labels(tac_proc)
    fallthrough_to_jump(tac_proc)
    normalize_labels(tac_proc)
    assert len(tac_proc.body) > 0
    assert tac_proc.body[0].opcode == 'label'
    lab_entry = tac_proc.body[0].arg1
    blocks = []
    cur = 0
    while cur < len(tac_proc.body):
        instr = tac_proc.body[cur]
        assert instr.opcode == 'label'
        bl = Block(instr.arg1)
        cur += 1
        while cur < len(tac_proc.body):
            instr = tac_proc.body[cur]
            if _enders.fullmatch(instr.opcode):
                break
            bl.body.append(instr)
            cur += 1
        while cur < len(tac_proc.body):
            instr = tac_proc.body[cur]
            if not _enders.fullmatch(instr.opcode):
                break
            bl.jumps.append(instr)
            cur += 1
        blocks.append(bl)
    return CFG(tac_proc.name, lab_entry, blocks)

# --------------------------------------------------------------------------------


def linearize(tac_proc, cfg):
    seen = set()
    wl = [cfg.lab_entry]
    schedule = []

    def emit(bl):
        nonlocal schedule
        schedule.append(tac.Instr(None, 'label', (bl.label, None)))
        for instr in bl.instrs():
            schedule.append(instr)
    ret_wl = []
    while len(wl) > 0:
        cur = wl.pop()
        if cur in seen:
            continue
        if cfg.out_degree(cur) == 0:
            # delay rets to end
            ret_wl.append(cur)
            continue
        seen.add(cur)
        bl = cfg[cur]
        emit(bl)
        wl.extend(cfg.successors(cur))
    while len(ret_wl) > 0:
        cur = ret_wl.pop()
        if cur in seen:
            continue
        seen.add(cur)
        emit(cfg[cur])
    tac_proc.body = schedule

# ------------------------------------------------------------------------------


def filter_liveset(lab, lset):
    for x in lset:
        if isinstance(x, tuple):
            if x[0] == lab:
                yield x[1]
        else:
            yield x


def recompute_liveness(cfg, livein, liveout):
    """Perform liveness analysis on the given cfg, storing the results in `livein' and `liveout'.
    Note: both `livein' and `liveout' are cleaned out before computing liveness."""
    livein.clear()
    liveout.clear()
    # initialize livein with the use sets
    for i in cfg.instrs():
        livein[i] = set(i.uses())
        liveout[i] = set()
    dirty = True

    def update_livein(i, j_livein):
        nonlocal dirty
        old_count = len(livein[i])
        old_set = str(livein[i])
        i_defs = set(i.defs())
        for x in j_livein:
            if ((isinstance(x, tuple) and x[1] in i_defs) or
                    x in i_defs):
                continue
            livein[i].add(x)
        if old_count != len(livein[i]):
            dirty = True
    while dirty:
        dirty = False
        for (li, i, lj, j) in cfg.instr_pairs(labeled=True):
            if li == lj:
                update_livein(i, livein[j])
            else:
                update_livein(i, filter_liveset(li, livein[j]))
    for li, i, lj, j in cfg.instr_pairs(labeled=True):
        liveout[i].update(filter_liveset(li, livein[j]))
    # fix the livein sets to remove tuples
    for i, li in livein.items():
        livein[i] = {x[1] if isinstance(x, tuple) else x
                     for x in li}

# ------------------------------------------------------------------------------


if __name__ == '__main__':
    import os
    from argparse import ArgumentParser
    ap = ArgumentParser(description='TAC library, parser, and interpreter')
    ap.add_argument('file', metavar='FILE', type=str,
                    nargs=1, help='A TAC file')
    ap.add_argument('-v', dest='verbosity', default=0, action='count',
                    help='increase verbosity')
    args = ap.parse_args()
    gvars, procs = dict(), dict()
    for tlv in tac.load_tac(args.file[0]):
        if isinstance(tlv, tac.Proc):
            cfg = infer(tlv)
            # Uncomment these two lines to have it generate a PDF of the CFG
            #   (Requires the graphviz toolkit: https://graphviz.org)
            # cfg.write_dot(args.file[0])
            # os.system(f'dot -Tpdf -O {args.file[0]}.{tlv.name[1:]}.dot')
            if args.verbosity > 0:
                linearize(tlv, cfg)
                print(tlv)

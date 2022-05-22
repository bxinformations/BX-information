#!/usr/bin/env python3

"""
Three Address Code (TAC) intermediate representation

Also includes a parser and interpreter.
"""

import json
import ply.yacc
import ply.lex
from io import StringIO

# ------------------------------------------------------------------------------

opcode_kinds = {
    'nop': 'NNN',
    'jmp': 'NLN',
    'jz': 'NVL', 'jnz': 'NVL', 'jl': 'NVL', 'jle': 'NVL',
    'jnl': 'NVL', 'jnle': 'NVL',
    'add': 'VVV', 'sub': 'VVV', 'mul': 'VVV', 'div': 'VVV',
    'mod': 'VVV', 'neg': 'VVN', 'and': 'VVV', 'or': 'VVV',
    'xor': 'VVV', 'not': 'VVN', 'shl': 'VVV', 'shr': 'VVV',
    'const': 'VIN', 'copy': 'VVN',
    'label': 'NLN',
    'param': 'NIV', 'call': 'OGI', 'ret': 'NON',
    'phi': 'VFN',
}
opcodes = frozenset(opcode_kinds.keys())


class Instr:
    __slots__ = ('dest', 'opcode', 'arg1', 'arg2')

    def __init__(self, dest, opcode, args):
        """Create a new TAC instruction with given `opcode' (must be non-None).
        The other three arguments, `dest', 'arg1', and 'arg2' depend on what
        the opcode is.

        Raises ValueError if attempting to create an invalid Instr."""
        self.dest = dest
        self.opcode = opcode
        self.arg1 = None if len(args) < 1 else args[0]
        self.arg2 = None if len(args) < 2 else args[1]
        self._check()

    def __hash__(self):
        return hash(id(self))

    def __eq__(self, other):
        return self is other

    @staticmethod
    def _isvar(thing):
        return isinstance(thing, str) and \
            len(thing) > 0 and \
            (thing[0] == '%' or thing[0] == '@')

    @staticmethod
    def _isint(thing):
        return isinstance(thing, int)

    @staticmethod
    def _islabel(thing):
        return isinstance(thing, str) and \
            thing.startswith('%.L')

    @staticmethod
    def _isglobal(thing):
        return isinstance(thing, str) and \
            thing.startswith('@')

    @staticmethod
    def _isphiargs(thing):
        return (isinstance(thing, dict) and
                all(Instr._isvar(x) for x in thing.values()))

    @staticmethod
    def _isvalid(thing, k):
        if k == 'N':
            return thing == None
        if k == 'I':
            return Instr._isint(thing)
        if k == 'V':
            return Instr._isvar(thing)
        if k == 'L':
            return Instr._islabel(thing)
        if k == 'G':
            return Instr._isglobal(thing)
        if k == 'F':
            return Instr._isphiargs(thing)
        if k == 'O':
            return thing == None or Instr._isvar(thing)
        return ValueError(f'Unknown argument kind: {k}')

    def _check(self):
        """Perform a well-formedness check on this instruction"""
        if self.opcode not in opcodes:
            raise ValueError(f'bad tac.Instr opcode: {self.opcode}')
        kind = opcode_kinds[self.opcode]
        if not self._isvalid(self.dest, kind[0]):
            raise ValueError(
                f'bad tac.Instr/{self.opcode} destination: {self.dest}')
        if not self._isvalid(self.arg1, kind[1]):
            raise ValueError(f'bad tac.Instr/{self.opcode} arg1: {self.arg1}')
        if not self._isvalid(self.arg2, kind[2]):
            raise ValueError(f'bad tac.Instr/{self.opcode} arg2: {self.arg2}')

    def __repr__(self):
        return f'Instr.load({self.js_obj})'

    def __str__(self):
        result = StringIO()
        if self.opcode == 'label':
            result.write(f'{self.arg1}:')
        elif self.opcode == 'phi':
            result.write(f'  {self.dest} = phi(')
            result.write(
                ', '.join(f'{lab}:{tmp}' for lab, tmp in self.arg1.items()))
            result.write(');')
        else:
            result.write('  ')
            if self.dest != None:
                result.write(f'{self.dest} = ')
            result.write(f'{self.opcode}')
            if self.arg1 != None:
                result.write(f' {self.arg1}')
                if self.arg2 != None:
                    result.write(f', {self.arg2}')
            result.write(';')
        return result.getvalue()

    @staticmethod
    def _istemp(thing):
        return (isinstance(thing, str) and
                thing.startswith('%') and
                not thing.startswith('%.L'))

    def defs(self):
        """Returns an iterator over the temporaries defined in this instruction"""
        if self._istemp(self.dest):
            yield self.dest

    def uses(self):
        """Returns an iterator over the temporaries used in this instruction.
        Each item of the terator is either a temporary by itself or a
        2-tuple of the form (label, temporary) that corresponds to an
        argument of a phi-function."""
        if self._istemp(self.arg1):
            yield self.arg1
        if self._istemp(self.arg2):
            yield self.arg2
        if self.opcode == 'phi':
            for l, t in self.arg1.items():
                if self._istemp(t):
                    yield (l, t)

    def rewrite_temps(self, rew):
        """Apply `rew' to rewrite the temps in this instruction. If `rew' is a
        dictionary, then it will be used to look up the new mapping of
        a temporary (if one exists), defaulting to no change if there
        is no mapping. If `rew' is a function, then every temporary t will
        be mapped to `rew(t)'."""
        if isinstance(rew, dict):
            def lookup(t): return rew.get(t, t)
        else:
            lookup = rew
        if self._istemp(self.dest):
            self.dest = lookup(self.dest)
        if self._istemp(self.arg1):
            self.arg1 = lookup(self.arg1)
        if self._istemp(self.arg2):
            self.arg2 = lookup(self.arg2)
        if self.opcode == 'phi':
            for l, t in self.arg1.items():
                self.arg1[l] = lookup(t)

    @staticmethod
    def load(js_obj):
        opcode = js_obj.get('opcode', None)
        assert opcode is not None
        args = js_obj.get('args', ())
        result = js_obj.get('result', None)
        return Instr(result, opcode, args)

    @property
    def js_obj(self):
        """A basic Python object ready to JSONify with json.dump()"""
        return {'opcode': self.opcode,
                'args': (self.arg1, self.arg2),
                'result': self.dest}


class Proc:
    def __init__(self, name, t_args, body):
        self.name = name
        self.body = body or []
        self.t_args = tuple(t_args)

    def __str__(self):
        result = StringIO()
        result.write(f'proc {self.name}({", ".join(self.t_args)}):\n')
        for instr in self.body:
            print(instr, file=result)
        return result.getvalue()

    @staticmethod
    def load(js_obj):
        name = js_obj.get('proc', None)
        if not name:
            return
        assert isinstance(name, str)
        assert name.startswith('@')
        args = js_obj.get('args', ())
        body = [Instr.load(i) for i in js_obj.get('body', [])]
        return Proc(name, args, body)

    @property
    def js_obj(self):
        return {'proc': self.name,
                'args': list(self.t_args),
                'body': [i.js_obj for i in self.body]}


class Gvar:
    def __init__(self, name, value):
        self.name = name
        self.value = value

    def __str__(self):
        return f'var {self.name} = {self.value};\n'

    @staticmethod
    def load(js_obj):
        name = js_obj.get('var', None)
        if not name:
            return
        assert isinstance(name, str)
        assert name.startswith('@')
        init = js_obj.get('init', ())
        return Gvar(name, init)

    @property
    def js_obj(self):
        return {'var': self.name,
                'init': self.value}

# ------------------------------------------------------------------------------


class Lexer:
    reserved = {
        'var': 'VAR',
        'proc': 'PROC',
    }

    tokens = (
        'NUM64', 'TEMP', 'GSYM', 'LABEL', 'OPCODE',
        'EQ', 'COMMA', 'SEMICOLON', 'COLON', 'LPAREN', 'RPAREN',
    ) + tuple(reserved.values())

    t_ignore = ' \t\f\v\r'

    t_TEMP = r'%(0|[1-9][0-9]*|[A-Za-z][A-Za-z0-9_]*)'
    t_GSYM = r'@[A-Za-z_][A-Za-z0-9_]*'
    t_LABEL = r'%\.L[A-Za-z0-9_]*'

    t_EQ = r'='
    t_COMMA = r','
    t_SEMICOLON = r';'
    t_COLON = r':'
    t_LPAREN = r'\('
    t_RPAREN = r'\)'

    def t_newline(self, t):
        r'\n|//[^\n]*\n?'
        t.lexer.lineno += 1

    def t_OPCODE(self, t):
        r'[A-Za-z_][A-Za-z0-9_]*'
        t.type = self.reserved.get(t.value, 'OPCODE')
        return t

    def t_NUM64(self, t):
        r'0|-?[1-9][0-9]*'
        t.value = int(t.value)
        if t.value & 0xffffffffffffffff != t.value:
            print(f'{self.provenance}:{t.lineno}:'
                  f'Error: numerical literal {t.value} not in [{-1<<63}, {1<<63})')
            raise SyntaxError('immint')
        return t

    def t_error(self, t):
        print(f'{self.provenance}:{t.lineno}:'
              f'Warning: skipping illegal character: {t.value[0]}')
        t.lexer.skip(1)

    def __init__(self, text, provenance="<unknown>"):
        self.text = text
        self.provenance = provenance
        self.lexer = ply.lex.lex(module=self)
        self.lexer.input(self.text)

# ------------------------------------------------------------------------------


class Parser:
    tokens = Lexer.tokens

    def p_program(self, p):
        '''program : program gvar
                   | program proc
                   | '''
        if len(p) == 1:
            p[0] = []
        else:
            p[0] = p[1]
            p[0].append(p[2])

    def p_gvar(self, p):
        '''gvar : VAR GSYM EQ NUM64 SEMICOLON'''
        p[0] = Gvar(p[2], p[4])

    def p_proc(self, p):
        '''proc : PROC GSYM procparams COLON instrs'''
        p[0] = Proc(p[2], p[3], p[5])

    def p_procparams(self, p):
        '''procparams : LPAREN argtemps RPAREN
                      |'''
        p[0] = () if len(p) == 1 else p[2]

    def p_argtemps(self, p):
        '''argtemps : argtemps1
                    | '''
        p[0] = () if len(p) == 1 else p[1]

    def p_argtemps1(self, p):
        '''argtemps1 : TEMP
                     | argtemps1 COMMA TEMP'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1]
            p[0].append(p[3])

    def p_instrs(self, p):
        '''instrs : instrs instr
                  | '''
        if len(p) == 1:
            p[0] = []
        else:
            p[0] = p[1]
            p[0].append(p[2])

    def p_instr(self, p):
        '''instr : lhs OPCODE args SEMICOLON'''
        lhs, opcode, args = p[1], p[2], p[3]
        p[0] = Instr(lhs, opcode, args)

    def p_label(self, p):
        '''instr : LABEL COLON'''
        p[0] = Instr(None, 'label', [p[1]])

    def p_lhs(self, p):
        '''lhs : TEMP EQ
               | GSYM EQ
               | '''
        p[0] = None if len(p) == 1 else p[1]

    def p_args(self, p):
        '''args : arg COMMA arg
                | arg
                | '''
        if len(p) == 1:
            p[0] = ()
        elif len(p) == 2:
            p[0] = (p[1],)
        else:
            p[0] = (p[1], p[3])

    def p_arg(self, p):
        '''arg : TEMP
               | NUM64
               | LABEL
               | GSYM'''
        p[0] = p[1]

    def p_error(self, p):
        if p:
            print(
                f'{self.lexer.provenance}:{p.lineno}:Error:syntax error at token {p.type}')
        raise RuntimeError('parsing')

    def __init__(self, lexer):
        self.lexer = lexer
        self.parser = ply.yacc.yacc(module=self, start='program')

    def parse(self):
        return self.parser.parse(lexer=self.lexer.lexer, tracking=True)

# ------------------------------------------------------------------------------


word_bytes = 8
word_bits = 8 * word_bytes
sign_mask = 1 << (word_bits - 1)
full_mask = (1 << word_bits) - 1


def untwoc(x):
    """Convert a 64-bit word in two's complement representation
    to a Python int"""
    return x - full_mask - 1 if x & sign_mask else x


def twoc(x):
    """Convert a Python int in range to a 64-bit word in two's
    complement representation"""
    return x & full_mask


binops = {
    'add': (lambda u, v: twoc(untwoc(u) + untwoc(v))),
    'sub': (lambda u, v: twoc(untwoc(u) - untwoc(v))),
    'mul': (lambda u, v: twoc(untwoc(u) * untwoc(v))),
    'div': (lambda u, v: twoc(int(untwoc(u) / untwoc(v)))),
    'mod': (lambda u, v: twoc(untwoc(u) - untwoc(v) * int(untwoc(u) / untwoc(v)))),
    'and': (lambda u, v: twoc(untwoc(u) & untwoc(v))),
    'or': (lambda u, v: twoc(untwoc(u) | untwoc(v))),
    'xor': (lambda u, v: twoc(untwoc(u) ^ untwoc(v))),
    'shl': (lambda u, v: twoc(untwoc(u) << untwoc(v))),
    'shr': (lambda u, v: twoc(untwoc(u) >> untwoc(v))),
}
unops = {
    'neg': (lambda u: twoc(-untwoc(u))),
    'not': (lambda u: twoc(~untwoc(u))),
}
jumps = {
    'jz':  (lambda k: k == 0),
    'jnz': (lambda k: k != 0),
    'jl':  (lambda k: untwoc(k) < 0),
    'jle': (lambda k: untwoc(k) <= 0),
    'jnl':  (lambda k: untwoc(k) >= 0),
    'jnle': (lambda k: untwoc(k) > 0),
}


class TempMap(dict):
    """Mapping temporaries to values"""

    def __init__(self, gvars):
        super().__init__()
        self.gvars = gvars

    def _valid_temp(self, tmp):
        return isinstance(tmp, str) and \
            (tmp.startswith('%') or tmp.startswith('@'))

    def _valid_value(self, val):
        return isinstance(val, int) and \
            0 <= val <= 0xffffffffffffffff

    def __getitem__(self, tmp):
        if tmp.startswith('@'):
            return self.gvars[tmp].value
        return super().__getitem__(tmp)

    def copy(self):
        tm = TempMap(self.gvars.copy())
        for k, v in super().items():
            tm[k] = v
        return tm

    def __setitem__(self, tmp, val):
        assert self._valid_temp(tmp)
        if not self._valid_value(val):
            raise RuntimeError(f'Illegal value: {val}: '
                               f'{-0x8000000000000000 <= val} '
                               f'{val < 0x8000000000000000}')
        if tmp.startswith('@'):
            self.gvars[tmp].value = val
        else:
            super().__setitem__(tmp, val)


def execute(gvars, procs, proc_name, args, **kwargs):
    show_proc = kwargs.get('show_proc', False)
    show_instr = kwargs.get('show_instr', False)
    only_decimal = kwargs.get('only_decimal', True)
    depth = kwargs.get('depth', 0)
    indent = '  ' * depth

    values = TempMap(gvars)
    proc = procs[proc_name]

    for i in range(len(proc.t_args)):
        values[proc.t_args[i]] = args[i]

    oldvalues = values.copy()

    proc_desc = f'{proc_name}({",".join(k + "=" + str(v) for k, v in values.items())})'
    if show_proc:
        print(f'// {indent}entering {proc_desc}')

    labels = dict()
    for i, instr in enumerate(proc.body):
        if instr.opcode != 'label':
            continue
        if instr.arg1 in labels:
            raise RuntimeError(f'Reused label {instr.arg1}')
        ni = i + 1  # next instruction index
        while ni < len(proc.body):
            if proc.body[ni].opcode != 'label':
                break
            ni += 1
        labels[instr.arg1] = ni

    lab_prev, lab_cur = None, proc_name
    pc = 0
    params = []
    while pc in range(len(proc.body)):
        instr = proc.body[pc]
        pc += 1

        if show_instr:
            print(f'// {indent}[{pc+1: 4d}] {instr}')
        if instr.opcode == 'nop':
            pass
        elif instr.opcode == 'label':
            lab_prev, lab_cur = lab_cur, instr.arg1
        elif instr.opcode == 'phi':
            for lab, tmp in instr.arg1.items():
                if lab == lab_prev:
                    values[instr.dest] = oldvalues[tmp]
                    break
            else:
                raise RuntimeError(f'cannot resolve phi: '
                                   f'came from {lab_prev}, '
                                   f'can only handle [{",".join(instr.arg1.keys())}]')
        elif instr.opcode == 'jmp':
            if instr.arg1 not in labels:
                raise RuntimeError(f'Unknown jump destination {instr.arg1}')
            lab_prev, lab_cur = lab_cur, instr.arg1
            oldvalues = values.copy()
            pc = labels[lab_cur]
        elif instr.opcode in jumps:
            k = values[instr.arg1]
            if instr.arg2 not in labels:
                raise RuntimeError(f'Unknown jump destination {instr.arg2}')
            if jumps[instr.opcode](k):
                lab_prev, lab_cur = lab_cur, instr.arg2
                oldvalues = values.copy()
                pc = labels[lab_cur]
        elif instr.opcode == 'const':
            if not isinstance(instr.arg1, int):
                print(f'Missing or bad argument: {instr.arg1}')
                raise RuntimeError
            values[instr.dest] = twoc(instr.arg1)
        elif instr.opcode == 'copy':
            values[instr.dest] = values[instr.arg1]
        elif instr.opcode == 'param':
            if not isinstance(instr.arg1, int) or instr.arg1 < 1:
                print(f'Bad argument to param: '
                      f'expecting int >= 1, got {instr.arg1}')
            # make params big enough to hold instr.arg1 items
            for _ in range(instr.arg1 - len(params)):
                params.append(None)
            params[instr.arg1 - 1] = values[instr.arg2]
        elif instr.opcode == 'call':
            if instr.arg1.startswith('@__bx_print'):
                if len(params) != 1:
                    raise RuntimeError(f'Bad number of arguments to print(): '
                                       f'expected 1, got {len(params)}')
                if instr.arg1 == '@__bx_print_int':
                    u = params[0]
                    if only_decimal:
                        print(str(untwoc(u)))
                    else:
                        print(f'{untwoc(u): 20d}  0x{u:016x}  0b{u:064b}')
                elif instr.arg1 == '@__bx_print_bool':
                    print('false' if params[0] == 0 else 'true')
                else:
                    raise RuntimeError(
                        f'Unknown print() specialization: {instr.arg1}')
            else:
                if len(params) < instr.arg2:
                    raise RuntimeError(f'Bad number of arguments to {instr.arg1}(): '
                                       f'expected {instr.arg2}, got {len(params)}')
                kwargs['depth'] = depth + 1
                result = execute(gvars, procs, instr.arg1, params, **kwargs)
                if instr.dest:
                    values[instr.dest] = result
            params = []
        elif instr.opcode == 'ret':
            retval = None if instr.arg1 == None else values[instr.arg1]
            if show_proc:
                print(f'// {indent}{proc_desc} --> {retval}')
            return retval
        elif instr.opcode in binops:
            u = values[instr.arg1]
            v = values[instr.arg2]
            values[instr.dest] = binops[instr.opcode](u, v)
        elif instr.opcode in unops:
            u = values[instr.arg1]
            if instr.arg2 != None:
                print(f'Unary operator {instr.opcode} has two arguments!')
                raise RuntimeError
            values[instr.dest] = unops[instr.opcode](u)
        else:
            print(f'Unknown opcode {instr.opcode}')
            raise RuntimeError
    print(f'// {indent}{proc_desc} --> NONE')

# --------------------------------------------------------------------------------


def load_tac(tac_file):
    """Load the TAC instructions from the given `tac_file'"""
    with open(tac_file, 'r') as fp:
        if tac_file.endswith('.tac'):
            text = fp.read()
            lexer = Lexer(text, tac_file)
            parser = Parser(lexer)
            return parser.parse()
        elif tac_file.endswith('.tac.json'):
            return [Gvar.load(obj) or Proc.load(obj)
                    for obj in json.load(fp)]
        else:
            raise ValueError(f'TAC file must be a .tac or a .tac.json')


if __name__ == '__main__':
    from argparse import ArgumentParser
    ap = ArgumentParser(description='TAC parser and interpreter')
    ap.add_argument('files', metavar='FILE', type=str, nargs='*',
                    help='A TAC file (.tac or .tac.json)')
    ap.add_argument('-v', dest='verbosity', default=0, action='count',
                    help='increase verbosity')
    ap.add_argument('--dump-json', dest='dump_json', action='store_true',
                    default=False,
                    help='Dump the TAC in JSON form (if needed)')
    ap.add_argument('--trace-procs', dest='trace_procs',
                    action='store_true', default=False,
                    help='Print enter/leave messages for procedure calls')
    ap.add_argument('--trace-instrs', dest='trace_instrs',
                    action='store_true', default=False,
                    help='Print the instructions as they are executed')
    ap.add_argument('--trace-all', dest='trace_all',
                    action='store_true', default=False,
                    help='Turn on all the trace-* options')
    ap.add_argument('--no-exec', dest='execute', action='store_false',
                    default=True,
                    help='Do not run the interpreter')
    args = ap.parse_args()
    if args.trace_all:
        args.trace_procs = True
        args.trace_instrs = True
    kwargs = dict(show_proc=args.trace_procs or args.verbosity > 3,
                  show_instr=args.trace_instrs or args.verbosity > 4,
                  only_decimal=args.verbosity <= 1)
    for srcfile in args.files:
        gvars, procs = dict(), dict()
        seen = set()
        prog = load_tac(srcfile)
        if args.dump_json and srcfile.endswith('.tac'):
            with open(srcfile + '.json', 'w') as fp:
                json.dump([tlv.js_obj for tlv in prog], fp, indent=2)
        for tlv in prog:
            if tlv.name in seen:
                raise RuntimeError(f'Repeated definition of {tlv.name}')
            seen.add(tlv.name)
            if isinstance(tlv, Proc):
                procs[tlv.name] = tlv
            else:
                gvars[tlv.name] = tlv
        if args.execute:
            execute(gvars, procs, '@main', (), **kwargs)
        elif args.verbosity > 0:
            for gvar in gvars.values():
                print(gvar)
            for proc in procs.values():
                print(proc)

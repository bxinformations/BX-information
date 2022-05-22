#!/usr/bin/env python3

"""
This is a very simple TAC to x64 assembly pass. It only handles straightline
code and a single main() function.

Usage: python3 tac2asm.py tacfile.json
Produces: tacfile.s (assembly) and tacfile.exe (executable)
Requires: a working gcc
"""

import json
import sys
import os
from pathlib import Path

binops = {'add': 'addq',
          'sub': 'subq',
          'mul': (lambda ra, rb, rd: [f'movq {ra}, %rax',
                                      f'imulq {rb}',
                                      f'movq %rax, {rd}']),
          'div': (lambda ra, rb, rd: [f'movq {ra}, %rax',
                                      f'cqto',
                                      f'idivq {rb}',
                                      f'movq %rax, {rd}']),
          'mod': (lambda ra, rb, rd: [f'movq {ra}, %rax',
                                      f'cqto',
                                      f'idivq {rb}',
                                      f'movq %rdx, {rd}']),
          'and': 'andq',
          'or': 'orq',
          'xor': 'xorq',
          'shl': (lambda ra, rb, rd: [f'movq {ra}, %r11',
                                      f'movq {rb}, %rcx',
                                      f'salq %cl, %r11',
                                      f'movq %r11, {rd}']),
          'shr': (lambda ra, rb, rd: [f'movq {ra}, %r11',
                                      f'movq {rb}, %rcx',
                                      f'sarq %cl, %r11',
                                      f'movq %r11, {rd}'])}
unops = {'neg': 'negq',
         'not': 'notq'}

def lookup_temp(temp, temp_map):
  assert (isinstance(temp, str) and \
          temp[0] == '%' and \
          temp[1:].isnumeric()), temp
  return temp_map.setdefault(temp, f'{-8 * (len(temp_map) + 1)}(%rbp)')

def tac_to_asm(tac_instrs):
  """
  Get the x64 instructions correspondign to the TAC instructions
  """
  temp_map = dict()
  asm = []
  for instr in tac_instrs:
    opcode = instr['opcode']
    args = instr['args']
    result = instr['result']
    if opcode == 'nop':
      pass
    elif opcode == 'const':
      assert len(args) == 1 and isinstance(args[0], int)
      result = lookup_temp(result, temp_map)
      asm.append(f'movq ${args[0]}, {result}')
    elif opcode == 'copy':
      assert len(args) == 1
      arg = lookup_temp(args[0], temp_map)
      result = lookup_temp(result, temp_map)
      asm.append(f'movq {arg}, %r11')
      asm.append(f'movq %r11, {result}')
    elif opcode in binops:
      assert len(args) == 2
      arg1 = lookup_temp(args[0], temp_map)
      arg2 = lookup_temp(args[1], temp_map)
      result = lookup_temp(result, temp_map)
      proc = binops[opcode]
      if isinstance(proc, str):
        asm.extend([f'movq {arg1}, %r11',
                    f'{proc} {arg2}, %r11',
                    f'movq %r11, {result}'])
      else: asm.extend(proc(arg1, arg2, result))
    elif opcode in unops:
      assert len(args) == 1
      arg = lookup_temp(args[0], temp_map)
      result = lookup_temp(result, temp_map)
      proc = unops[opcode]
      asm.extend([f'movq {arg}, %r11',
                  f'{proc} %r11',
                  f'movq %r11, {result}'])
    elif opcode == 'print':
      assert len(args) == 1
      assert result == None
      arg = lookup_temp(args[0], temp_map)
      asm.extend([f'leaq .lprintfmt(%rip), %rdi',
                  f'movq {arg}, %rsi',
                  f'xorq %rax, %rax',
                  f'callq printf@PLT'])
    else:
      assert False, f'unknown opcode: {opcode}'
  stack_size = len(temp_map)
  if stack_size % 2 != 0: stack_size += 1 # 16 byte alignment for x64
  asm[:0] = [f'pushq %rbp',
             f'movq %rsp, %rbp',
             f'subq ${8 * stack_size}, %rsp'] \
  #  + [f'// {tmp} in {reg}' for (tmp, reg) in temp_map.items()]
  asm.extend([f'movq %rbp, %rsp',
              f'popq %rbp',
              f'xorq %rax, %rax',
              f'retq'])
  return asm

def compile_tac(fname):
  if fname.endswith('.tac.json'):
    rname = fname[:-9]
  elif fname.endswith('.json'):
    rname = fname[:-5]
  else:
    raise ValueError(f'{fname} does not end in .tac.json or .json')
  tjs = None
  with open(fname, 'rb') as fp:
    tjs = json.load(fp)
  assert isinstance(tjs, list) and len(tjs) == 1, tjs
  tjs = tjs[0]
  assert 'proc' in tjs and tjs['proc'] == '@main', tjs
  asm = ['\t' + line for line in tac_to_asm(tjs['body'])]
  asm[:0] = [f'\t.section .rodata',
             f'.lprintfmt:',
             f'\t.string "%ld\\n"',
             f'\t.text',
             f'\t.globl main',
             f'main:']
  xname = rname + '.exe'
  sname = rname + '.s'
  with open(sname, 'w') as afp:
    print(*asm, file=afp, sep='\n')
  print(f'{fname} -> {sname}')
  os.system(f'gcc -o {xname} {sname}')
  print(f'{sname} -> {xname}')

if __name__ == '__main__':
  if len(sys.argv) != 2:
    print(f'Usage: {sys.argv[0]} tacfile.tac.json')
    sys.exit(1)
  compile_tac(sys.argv[1])

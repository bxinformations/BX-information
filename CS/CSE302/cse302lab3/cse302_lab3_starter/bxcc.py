from bx2tac import bx2tac
from tac2x64 import tac2x64

import subprocess
import getopt
import sys

if __name__ == '__main__':
    opts, args = getopt.getopt(sys.argv[1:], '', [])
    filename = args[0]

    if not filename.endswith('.bx'):
        print(filename, 'Wrong file extention')
        raise TypeError

    with open(filename, 'r') as file:
        code = file.read()

    tac_file_name = filename.split('\\')[-1].split('.')[0] + '.json'
    tac_file = bx2tac(code, file=tac_file_name)

    with open(tac_file_name, 'w') as file:
        file.write(tac_file)

    x64_file = tac2x64(file_name = tac_file_name)

    x64_name = filename.split('\\')[-1].split('.')[0] + '.s'

    f_out = open(x64_name, 'w')
    for i in x64_file:
        f_out.write(i + '\n')
    f_out.close()

    cmd = ['gcc']
    exe_file = x64_name[:-1] + 'exe'
    runtime_file = 'bx_runtime.c'
    cmd.extend(["-o", exe_file, runtime_file, x64_name])
    result = subprocess.run(cmd)

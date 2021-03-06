from bx2tac import bx2tac
from tac2x64 import tac2x64
from tac_cfopt import tac_cf

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

    tac_file = bx2tac(code, file=filename)
    tac_file_name = filename.split('\\')[-1].split('.')[0] + '.tac.json'
    with open(tac_file_name, 'w') as file:
        file.write(tac_file)

    tac_optimize_file = tac_cf(tac_file_name)
    filename = f"{tac_file_name[:-4]}tac_optimized.json"

    x64_file = tac2x64(file_name=filename)

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

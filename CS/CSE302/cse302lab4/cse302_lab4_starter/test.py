#!/usr/bin/env python3

"""
Lab 4 Tester
"""

import os
import subprocess
import glob


def main():

    test = glob.glob('./examples/*.bx')
    print('TESTING ./examples/*.bx')
    for filename in test:
        # Compilation
        print(filename)
        cmd = ["python3", "./bxcc.py", filename]
        result = subprocess.run(cmd, stdout=open(
            os.devnull, "w"), stderr=subprocess.STDOUT)
        if result.returncode != 0:
            ValueError(f"error at {filename}")
            continue


if __name__ == '__main__':
    main()

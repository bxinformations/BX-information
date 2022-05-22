import sys
import getopt
import tac
import cfg
import ssagen
import json


def DSE(cfg_now):
    f = False
    livein = {}
    liveout = {}
    cfg.recompute_liveness(cfg_now, livein, liveout)
    saved_opcode = ['div', 'mod', 'call', 'param', 'ret']
    for bl in cfg_now._blockmap.values():
        for i in range(len(bl.body)):
            if bl.body[i].opcode in saved_opcode:
                continue
            if bl.body[i].dest == None:
                continue
            if not tac.Instr._istemp(bl.body[i].dest):
                continue
            if bl.body[i].dest in liveout[bl.body[i]]:
                continue

            bl.body[i].opcode = 'nop'
            f = True

        bl.body = list(filter(lambda instr: instr.opcode != 'nop', bl.body))
    return f


def copy_propagation(cfg_now):
    for bl in cfg_now._blockmap.values():
        for i in range(len(bl.body)):
            if bl.body[i].opcode == 'copy':
                used = bl.body[i].arg1
                if not tac.Instr._istemp(used):
                    continue

                tmp = bl.body[i].dest
                if not tac.Instr._istemp(tmp):
                    continue
                for blp in cfg_now._blockmap.values():
                    for j in blp.instrs():
                        if j.opcode == 'copy':
                            continue
                        if j.opcode == 'phi':
                            for arg in j.arg1.keys():
                                if j.arg1[arg] == tmp:
                                    j.arg1[arg] = used
                              
                        if j.arg1 == None:
                            continue
                        if j.arg1 == tmp:
                            j.arg1 = used

                        if j.arg2 == None:
                            continue
                        if j.arg2 == tmp:
                            j.arg2 = used

                bl.body[i].opcode = 'nop'

        bl.body = list(filter(lambda instr: instr.opcode != 'nop', bl.body))


if __name__ == "__main__":
    opts, args = getopt.getopt(sys.argv[1:], '-o', [])

    tac_file = args[1] if (len(opts) > 0 and opts[0][0] == '-o') else args[0]

    tac_now = tac.load_tac(tac_file)
    gvars, procs = dict(), dict()
    tac_new = []

    for decl in tac_now:
        if isinstance(decl, tac.Gvar):
            gvars[decl.name] = decl

        elif isinstance(decl, tac.Proc):
            cfg_now = cfg.infer(decl)
            ssagen.crude_ssagen(decl, cfg_now)
            copy_propagation(cfg_now)
            dse = True
            while dse:
               dse = DSE(cfg_now)

            cfg.linearize(decl, cfg_now)
            procs[decl.name] = cfg_now

        if len(opts) > 0 and opts[0][0] == '-o':
            tac_new.append(decl.js_obj)
        else:
            print(decl)

    if len(opts) > 0 and  opts[0][0] == '-o':
        tac_json = json.dumps(tac_new)
        with open(args[0], 'w') as file:
            file.write(tac_json)

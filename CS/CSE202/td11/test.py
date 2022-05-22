from NFA import *

grading_mode = False

def test1():
    nfa=NFA("(a)*b(c|(f|(e|g))*)*a")
    # clear lp/rp because of match_*_or
    nfa.lp=[-1 for _ in range(len(nfa.s))]
    nfa.rp=[-1 for _ in range(len(nfa.s))]
    nfa.left_right_match()
    if nfa.lp == [-1 for _ in range(len(nfa.s))]:
        print("Skipping left_right_match (unimplemented)")
        assert not grading_mode
        return
    exp_lp=[-1, -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, 8, -1, 5, -1, -1]
    exp_rp=[2, -1, -1, -1, -1, 18, -1, -1, 16, -1, -1, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1]
    if nfa.lp==exp_lp and nfa.rp==exp_rp:
        print("Success!")
    else:
        print("Error: expect lp and rp to be respectively")
        print(exp_lp)
        print(exp_rp)
        print("but obtained")
        print(nfa.lp)
        print(nfa.rp)
        assert not grading_mode

def test2():
    nfa=NFA("(a)*b(c|(f|(e|g))*)*a")
    if nfa.lp == [-1 for _ in range(len(nfa.s))]:
        print("Skipping left_right_match_or (unimplemented)")
        assert not grading_mode
        return
    exp_lp=[-1, -1, 0, -1, -1, -1, -1, 5, -1, -1, 8, -1, -1, 11, -1, 11, 8, -1, 5, -1, -1]
    exp_rp=[2, -1, -1, -1, -1, 18, -1, 18, 16, -1, 16, 15, -1, 15, -1, -1, -1, -1, -1, -1, -1]
    if nfa.lp==exp_lp and nfa.rp==exp_rp:
        print("Success!")
    else:
        print("Error: expect lp and rp to be respectively")
        print(exp_lp)
        print(exp_rp)
        print("but obtained")
        print(nfa.lp)
        print(nfa.rp)
        assert not grading_mode

def test3():
    nfa=NFA("c(.(a|b))*k(.)*")
    if nfa.dg.neigh == [[] for _ in range(len(nfa.dg.neigh))]:
        print("Skipping build_eps_links (unimplemented)")
        assert not grading_mode
        return
    output=nfa.dg.to_string()
    expected=[[], [9, 2], [], [6, 4], [], [7], [], [8], [9], [1, 10], [], [14, 12], [], [14], [11, 15], []]
    for idx,links in enumerate(expected):
        exp = sorted(links)
        res = sorted(nfa.dg.neigh[idx])
        if exp != res:
            print("Error: {} is expected to be linked to {} but is linked to {}".format(idx, exp, res))
            assert not grading_mode
            return
    print("Success!")

def test4():
    nfa=NFA("c(.(a|b))*k(.)*")
    if nfa.check_text("") is None:
        print("Skipping check_text (unimplemented)")
        assert not grading_mode
        return
    for (s,expected) in [("", False), ("ck",True), ("cxak", True), ("cxck", False),
                         ("cxak", True), ("cxaybzakzzzzzzz", True), ("cxaybza", False)]:
        output = nfa.check_text(s)
        if output != expected:
            print("Error on word {}: expected {} but got {}".format(s, expected, output))
            assert not grading_mode
            return
    print("Success!")

test1()
test2()
test3()
test4()

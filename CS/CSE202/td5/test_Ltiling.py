import Ltiling as til

import __main__
grading_mode = __main__.__dict__.get('grading_mode', False)
print("grading mode is {}".format(grading_mode))

def test1():
    print("testing middleL")
    tmp=til.middleL(4,5,7,15,9)
    if tmp is None:
        print("skipping middleL (unimplemented)")
        assert not grading_mode
        return
    tmp.sort()
    res=[(12, 14), (12, 15), (13, 15)]
    if tmp != res:
            print("Your function numer_iter returned the wrong result ({} instead of {})".format(tmp, res))
            assert not grading_mode
    tmp=til.middleL(2,1,3,4,5)	
    tmp.sort()
    res=[(2, 4), (2, 5), (3, 4)]	
    if tmp != res:
            print("Your function numer_iter returned the wrong result ({} instead of {})".format(tmp, res))
            assert not grading_mode
    print("done.")

def test2():
    print("testing lower_left_hole")
    # lower_left_hole
    tmp=til.lower_left_hole(4,5,7,15,9)
    if tmp is None:
        print("skipping lower_left_hole (unimplemented)")
        assert not grading_mode
        return
    res=(12,14)	
    if tmp != res:
            print("Your function numer_iter returned the wrong result ({} instead of {})".format(tmp, res))
            assert not grading_mode
    tmp=til.lower_left_hole(4,5,7,7,8)
    res=(7,8)
    if tmp != res:
            print("Your function numer_iter returned the wrong result ({} instead of {})".format(tmp, res))
            assert not grading_mode
    print("testing lower_right_hole")
    # lower_right_hole
    tmp=til.lower_right_hole(4,5,7,15,9)
    if tmp is None:
        print("skipping lower_right_hole (unimplemented)")
        assert not grading_mode
        return
    res=(15,9)
    if tmp != res:
            print("Your function numer_iter returned the wrong result ({} instead of {})".format(tmp, res))
            assert not grading_mode
    tmp=til.lower_right_hole(4,5,7,7,8)
    res=(13,14)	
    if tmp != res:
            print("Your function numer_iter returned the wrong result ({} instead of {})".format(tmp, res))
            assert not grading_mode
    print("testing upper_left_hole")
    # upper_left_hole
    tmp=til.upper_left_hole(4,5,7,6,18)
    if tmp is None:
        print("skipping upper_left_hole (unimplemented)")
        assert not grading_mode
        return
    res=(6,18)
    if tmp != res:
            print("Your function numer_iter returned the wrong result ({} instead of {})".format(tmp, res))
            assert not grading_mode
    tmp=til.upper_left_hole(4,5,7,7,8)
    res=(12,15)
    if tmp != res:
            print("Your function numer_iter returned the wrong result ({} instead of {})".format(tmp, res))
            assert not grading_mode
    print("testing upper_right_hole")
    # upper_right_hole
    tmp=til.upper_right_hole(4,5,7,15,19)
    if tmp is None:
        print("skipping lower_left_hole (unimplemented)")
        assert not grading_mode
        return
    res=(15,19)
    if tmp != res:
            print("Your function numer_iter returned the wrong result ({} instead of {})".format(tmp, res))
            assert not grading_mode
    tmp=til.upper_right_hole(4,5,7,7,8)
    res=(13,15)
    if tmp != res:
            print("Your function numer_iter returned the wrong result ({} instead of {})".format(tmp, res))
            assert not grading_mode
    print("done.")

def test3():
    print("testing tile")
    til.Llist=[]
    til.tile(3,1,2,6,4)
    tmp=til.Llist
    if tmp==[]:
        print("skipping tile (unimplemented)")
        assert not grading_mode
        return
    for L in tmp:
            L.sort()
    tmp.sort()	
    res=[[(1, 2), (1, 3), (2, 2)], [(1, 4), (1, 5), (2, 5)], [(1, 6), (1, 7), (2, 6)], [(1, 8), (1, 9), (2, 9)], [(2, 3), (2, 4), (3, 3)], [(2, 7), (2, 8), (3, 8)], [(3, 2), (4, 2), (4, 3)], [(3, 4), (3, 5), (4, 4)], [(3, 6), (3, 7), (4, 7)], [(3, 9), (4, 8), (4, 9)], [(4, 5), (4, 6), (5, 6)], [(5, 2), (5, 3), (6, 2)], [(5, 4), (5, 5), (6, 5)], [(5, 7), (6, 6), (6, 7)], [(5, 8), (5, 9), (6, 9)], [(6, 3), (7, 3), (7, 4)], [(6, 8), (7, 7), (7, 8)], [(7, 2), (8, 2), (8, 3)], [(7, 5), (8, 4), (8, 5)], [(7, 6), (8, 6), (8, 7)], [(7, 9), (8, 8), (8, 9)]]
    if tmp != res:
        print("Your function tile returned the wrong result ({} instead of {})".format(tmp, res))
        assert not grading_mode
    print("done.")

test1()
test2()
test3()

# -*- coding: utf-8 -*-
import peaks as peak

import __main__
grading_mode = __main__.__dict__.get('grading_mode', False)
print("grading mode is {}".format(grading_mode))

def test_generic(f,inputs,outputs,name=""):
    print("testing {}".format(name))
    for i in range(len(inputs)):
        x=inputs[i]
        y1=outputs[i]
        y2=f(x)
        if y2 is None:
            print("skipping {} (unimplemented)".format(name))
            assert not grading_mode
            return
        if y1 != y2:
            print("Your function returned the wrong answer ({} instead of {}) on {}".format(y2, y1, x))
            assert not grading_mode
    print("done.")
           
def test1():
    inputs=[[2],[2,2],[2,3],[2,3,5],[1,2,3,4,5,4,6,2,7]]
    outputs=[0,0,1,2,4]
    test_generic(peak.peak_naive,inputs,outputs,"peak_naive")
    
def tab_unique_peak(n1,n2):
    res=[i for i in range(n1)]+[n1-i for i in range(n2)]
    return res
  
def test2():
    inputs=[[1],[2,3],[3,2],tab_unique_peak(11,3),tab_unique_peak(10,6)]
    outputs=[0,1,0,11,10] 
    test_generic(peak.peak,inputs,outputs,"peak")
    
def test3():
    print("testing is_peak")
    M=[[1,3,3,2],[2,2,6,4],[2,5,3,4]]
    output= [(0, 1), (1, 0), (1, 2), (2, 1), (2, 3)]
    if peak.is_peak(M, 0, 0) is None:
        print("skipping is_peak (unimplemented)")
        assert not grading_mode
        return
    
    I=len(M)
    J=len(M[0]) 
    res=[]
    for i in range(I):
        for j in range(J):
            if peak.is_peak(M,i,j):
                res+=[(i,j)]
    if res!=output:
        print("Error: on matrix ")
        print(M)
        print("the list of peaks is ", output)
        print("but your algorithm returned ", res)
        assert not grading_mode
    print("done.")
     
def test4():
    inputs=[[[4]],[[1,2],[2,2]],[[1,3,4,5],[2,8,10,9],[3,12,14,2]]]
    outputs=[(0, 0), (0, 1), (2, 2)] 
    test_generic(peak.peak2d_naive,inputs,outputs,"peak2d_naive")
    
def matrix_unique_peak(I,J,i0,j0):
    return [[min(i,2*i0-i)+min(j,2*j0-j) for j in range(J)] for i in range(I)]    
    
def test5():
    print("testing pivot")
    M=matrix_unique_peak(5,6,3,2)
    for params,res in [((0,5,0,6),(3,2)),((4,5,2,6),(4,2)),((1,3,1,4),(2,2)),((0,3,1,2),(2,1))]:
        x=peak.pivot(M,*params)
        if x is None:
            print("skipping pivot (unimplemented)")
            assert not grading_mode
            return
        if x != res:
            print("Error: on input matrix:")
            print(M)
            print("The expected pivot for F{} is {} but you returned {}".format(params, res, x))
    print("done.")

def test6():
    print("testing peak2d")
    for params,res in [((15,16,3,11),(3,11)), ((5,6,4,5),(4,5)), ((14,1,10,0),(10,0))]:
        M=matrix_unique_peak(*params)
        x=peak.peak2d(M)
        if x is None:
            print("skipping peak2d (unimplemented)")
            assert not grading_mode
            return
        if x!=res:
            print("Error: on input matrix:")
            print(M)
            print("The unique peak is at {} but you returned {}".format(res, x))  
    print("done.")

test1()
test2()
test3()
test4()
test5()
test6()

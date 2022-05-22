import random
import matplotlib.pyplot as plt

Llist=[] # the list of triples giving the tiling by L-shapes

## QUESTION 1

def middleL(n,i,j,a,b): # returns the triple giving the middle L for the (n,i,j,a,b) punctured grid
    l = i + (1 << (n - 1))
    r = j + (1 << (n - 1))
    if (a < l) and (b < r):
        return [(l, r), (l - 1, r), (l, r - 1)]
    if (a >= l) and (b < r):
        return [(l, r), (l - 1, r), (l - 1, r - 1)]
    if (a < l) and (b >= r):
        return [(l, r), (l, r - 1), (l - 1, r - 1)]
    return [(l - 1, r - 1), (l - 1, r), (l, r - 1)]

## QUESTION 2

def lower_left_hole(n,i,j,a,b): # returns the coordinates of the hole of the lower left quadrant
    l = i + (1 << (n - 1))
    r = j + (1 << (n - 1))
    if (a < l) and (b < r) and (a < l + (1 << (n - 1))) and (b < r + (1 << (n - 1))):
        return (a, b)
    return (l - 1, r - 1)

def lower_right_hole(n,i,j,a,b): # returns the coordinates of the hole of the lower right quadrant
    l = i + (1 << (n - 1))
    r = j + (1 << (n - 1))
    if (a >= l) and (b < r) and (a < l + (1 << (n - 1))) and (b < r + (1 << (n - 1))):
        return (a, b)
    return (l, r - 1)

def upper_left_hole(n,i,j,a,b): # returns the coordinates of the hole of the upper left quadrant
    l = i + (1 << (n - 1))
    r = j + (1 << (n - 1))
    if (a < l) and (b >= r) and (a < l + (1 << (n - 1))) and (b < r + (1 << (n - 1))):
        return (a, b)
    return (l - 1, r)

def upper_right_hole(n,i,j,a,b): # returns the coordinates of the hole of the upper right quadrant
    l = i + (1 << (n - 1))
    r = j + (1 << (n - 1))
    if (a >= l) and (b >= r) and (a < l + (1 << (n - 1))) and (b < r + (1 << (n - 1))):
        return (a, b)
    return (l, r)

## QUESTION 3

def tile(n,i,j,a,b):
    global Llist
    if (n == 0):
        return []
    Llist = Llist + [middleL(n, i, j, a, b)]

    c, d = lower_left_hole(n,i,j,a,b)
    tile(n - 1, i, j, c, d)
    c, d = lower_right_hole(n,i,j,a,b)    
    tile(n - 1, i + (1 << (n - 1)), j, c, d)
    c, d = upper_left_hole(n,i,j,a,b)
    tile(n - 1, i, j + (1 << (n - 1)), c, d)
    c, d = upper_right_hole(n,i,j,a,b)
    tile(n - 1, i + (1 << (n - 1)), j + (1 << (n - 1)), c, d)

## FUNCTION (GIVEN) TO DISPLAY A TILING OF SIZE n (WITH THE HOLE POSITION CHOSEN AT RANDOM)

def display_tiling_with_random_hole(n):
    global Llist
    Llist=[]
    N=2**n
    tile(n,0,0,random.randrange(N),random.randrange(N))
    #print(Llist)	
    data=[[[0,0,0] for _ in range(N)] for _ in range(N)]
    for L in Llist:
        r=random.randrange(256)
        g=random.randrange(256)
        b=random.randrange(256)
        for entry in L:
            data[entry[0]][entry[1]]=[r,g,b]
    #print(data)
    plt.imshow(data,origin='lower')
    plt.show()

## CALL TO THE DISPLAY FUNCTION (UNCOMMENT ONCE TESTS ARE OK)

# display_tiling_with_random_hole(2)

# C(N) = 4C(N / 2) + O(1), C(N) = O(N^2)
# -*- coding: utf-8 -*-

# the input is represented by a list of (left,height,right)
# the output (skyline) is represented by an ordered list of (left,height) and always ends with 
# a (left,0) entry

def rects_height_at(rects, x):
    """
    given a list of rectangles and a position, return h(x)=max{y:(l,r,y) in rects and l<=x<r}
    note that the inequality l<=x<r is assymetric: large on the left and strict on the right
    """
    res = 0
    for i in range(len(rects)):
        l, h, r = rects[i]
        if (l <= x) and (x < r):
            res = max(h, res)
    return res

def simplify_skyline(skyline):
    """simplify a skyline by removing redundant entries"""
    if (len(skyline) == 0):
        return skyline
    res = []
    res.append(skyline[0])
    for i in range(1, len(skyline)):
        x, h = skyline[i]
        x_pre, h_pre = skyline[i - 1]
        if (h != h_pre):
            res.append((x, h))
    return res

def skyline_naive(rects):
    """computes the skyline in O(n^2)"""
    skyline = []
    ri = 0
    for i in range(len(rects)):
        l, h, r = rects[i]
        ri = max(ri, r)
        h_l = rects_height_at(rects, l)
        h_r = rects_height_at(rects, r)
        skyline.append((l, h_l))
        skyline.append((r, h_r))

    for i in range(len(skyline)):
        for j in range(i, len(skyline)):
            if (skyline[i][0] > skyline[j][0]):
                skyline[i], skyline[j] = skyline[j], skyline[i]
            
    skyline = simplify_skyline(skyline)
    return skyline

def merge_skylines(sky1, sky2):
    """merge two skylines"""
    n = len(sky1)
    m = len(sky2)
    sky = []
    i = 0
    j = 0
    r = 0
    while (i < n) and (j < m):
        if (i == 0) and (j == 0):
            if (sky1[i][0] == sky2[j][0]):
                sky.append((sky1[i][0], max(sky1[i][1], sky2[j][1])))
                i += 1
                j += 1
                r = max(sky1[i][0], sky2[j][0])
            if (sky1[i][0] > sky2[j][0]):
                sky.append((sky2[j][0], sky2[j][1]))
                j += 1
                r = max(r, sky2[j][0])
            if (sky1[i][0] < sky2[j][0]):
                sky.append((sky1[i][0], sky1[i][1]))
                i += 1
                r = max(r, sky1[i][0])

        while (sky1[i][0] < r) and (sky1[i][1] <= sky[len(sky) - 1][1]) and (i < n):
            if (sky1[i][1] == sky[len(sky) - 1][1]) and (i + 1 < n):
                r = max(sky1[i + 1][0], r)
            i += 1
            if (i == n):
                break
        while (sky2[j][0] < r) and (sky2[j][1] <= sky[len(sky) - 1][1]) and (j < m):
            if (sky2[j][1] == sky[len(sky) - 1][1]) and (j + 1 < m):
                r = max(sky2[j + 1][0], r)
            j += 1
            if (j == m):
                break
        if (sky1[i][0] == sky2[j][0]):
            sky.append((sky1[i][0], max(sky1[i][1], sky2[j][1])))
            i += 1
            j += 1
            r = max(sky1[i + 1][0], sky2[j + 1][0])
        elif (sky1[i][0] > sky2[j][0]):
            sky.append((sky2[j][0], max(sky2[j][1], sky1[i - 1][1])))
            j += 1
            if (j < m):
                r = max(r, sky2[j][0])
        elif (sky1[i][0] < sky2[j][0]):
            sky.append((sky1[i][0], max(sky2[i][1], sky1[j - 1][1])))
            i += 1
            if (i < n):
                r = max(r, sky1[i][0])
    while (i < n):
        sky.append(sky1[i])
        i += 1
    while (j < m):
        sky.append(sky2[j])
        j += 1
    return sky


def skyline_dac(rects):
    if (len(rects) == 1):
        return [(rects[0][0], rects[0][1])]
    else:
        return merge_skylines(skyline_dac(rects[0 : len(rects) >> 1], rects[len(rects >> 1) : len(rects)]))

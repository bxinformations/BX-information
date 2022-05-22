# -*- coding: utf-8 -*-
import skyline as sky
import random
import time

import __main__
grading_mode = __main__.__dict__.get('grading_mode', False)
print("grading mode is {}".format(grading_mode))

rects1 = [(1, 11, 5), (2, 6, 7), (3, 13, 9), (12, 7, 16), (14, 3, 25), (19, 18, 22), (23, 13, 29), (24, 4, 28)]
heights1 = [(0, 0), (1,11), (2,11), (3,13), (4,13), (8,13), (9,0)]
sky1_red = [(1, 11), (2, 11), (3, 13), (5, 13), (7, 13), (9, 0), (12, 7), (14, 7), (16, 3), (19, 18), (22, 3),
            (23, 13), (24, 13), (25, 13), (28, 13), (29, 0)]
sky1 = [(1, 11), (3, 13), (9, 0), (12, 7), (16, 3), (19, 18), (22, 3), (23, 13), (29, 0)]

rects2 = [(2,10,9), (3, 15, 7), (5, 12, 12), (15, 10, 20), (19, 8, 24)]
sky2 = [(2, 10), (3, 15), (7, 12), (12, 0), (15, 10), (20, 8), (24, 0) ]

rects3 = [(404, 58, 752), (180, 96, 645), (102, 40, 443), (186, 42, 280), (323, 65, 819), (430, 9, 882), (419, 4, 720), (259, 27, 375), (268, 73, 531), (144, 12, 306), (19, 71, 232), (465, 34, 723), (329, 44, 471), (475, 73, 827), (94, 58, 443), (29, 89, 216), (393, 14, 655), (35, 36, 410), (450, 60, 514), (408, 45, 888), (369, 1, 421), (210, 58, 490), (5, 21, 39), (255, 64, 363), (162, 69, 368), (82, 35, 495), (158, 89, 473), (351, 64, 611), (380, 12, 470), (117, 27, 547), (435, 16, 632), (5, 7, 282), (259, 39, 712), (351, 85, 705), (73, 95, 526), (141, 35, 429), (146, 42, 409), (87, 27, 524), (403, 54, 767), (118, 64, 309)]
sky3 = [(5, 21), (19, 71), (29, 89), (73, 95), (180, 96), (645, 85), (705, 73), (827, 45), (888, 0)]

rects4 = [(249, 74, 250), (399, 96, 702), (76, 60, 476), (280, 11, 327), (117, 51, 487), (6, 73, 493), (74, 17, 422), (35, 51, 120), (25, 69, 284), (246, 65, 278)]
sky4 = [(6, 73), (249, 74), (250, 73), (399, 96), (702, 0)]

rects5 = [(349, 70, 590), (84, 19, 91), (14, 54, 371), (171, 86, 477), (80, 36, 403), (38, 26, 271), (327, 86, 522), (78, 29, 365), (87, 90, 222), (95, 93, 167), (295, 44, 380), (126, 68, 441), (158, 41, 589), (447, 61, 666), (116, 18, 587), (393, 29, 691), (442, 82, 799), (482, 96, 750), (365, 43, 548), (271, 99, 686), (312, 23, 718), (454, 43, 952), (442, 65, 894), (24, 16, 191), (421, 61, 538), (487, 27, 642), (420, 25, 529), (38, 54, 351), (222, 78, 228), (328, 34, 497), (164, 0, 447), (26, 22, 423), (41, 35, 320), (302, 2, 798), (153, 67, 246), (401, 62, 894), (163, 40, 464), (7, 28, 200), (476, 42, 878), (172, 38, 284), (205, 12, 305), (387, 60, 582), (187, 36, 282), (365, 67, 545), (130, 87, 333), (285, 91, 784), (310, 35, 359), (148, 36, 598), (221, 64, 235), (18, 79, 141), (478, 48, 648), (159, 14, 279), (31, 43, 131), (67, 86, 303), (2, 53, 355), (187, 90, 501), (482, 61, 607), (250, 55, 721), (487, 48, 578), (430, 85, 699)]
sky5 = [(2, 53), (14, 54), (18, 79), (67, 86), (87, 90), (95, 93), (167, 90), (271, 99), (686, 96), (750, 91), (784, 82), (799, 65), (894, 43), (952, 0)]

merge1_sky1 = [(1,5), (5, 2), (10, 3), (11, 0)]
merge1_sky2 = [(2,3), (3,7), (6, 1), (8, 2), (12, 5), (13, 0)]
merge1_res = [(1,5), (3, 7), (6, 2), (10, 3), (11, 2), (12, 5), (13, 0)]

merge2_sky1 = [(1, 11), (3, 13), (9, 0), (12, 7), (16, 0)]
merge2_sky2 = [(14, 3), (19, 18), (22, 3), (23, 13), (29, 0)]
merge2_res = [(1, 11), (3, 13), (9, 0), (12, 7), (16, 3), (19, 18), (22, 3), (23, 13), (29, 0)]

merge3_sky1 = [(1, 10), (3, 5), (8, 0)]
merge3_sky2 = [(1,20), (3, 2), (10,0)]
merge3_res = [(1, 20), (3, 5), (8, 2), (10, 0)]

def test_height_at():
    print("testing rects_height_at...")
    if sky.rects_height_at(rects1,0) is None:
        print("skipping rects_height_at (unimplemented)")
        assert not grading_mode
        return
    for (x,h) in heights1:
        res =sky.rects_height_at(rects1,x)
        if res != h:
            print("your function rects_height_at returned the wrong result on {}:".format(rects1))
            print("at position x={} it returned a height of {} instead of {}".format(x, res, h))
    print("done.")

def test_simplify():
    print("testing simplify_skyline...")
    for (sky_red, sk) in [(sky1_red, sky1)]:
        res =  sky.simplify_skyline(sky_red)
        if res is None:
            print("skipping simplify_skyline (unimplemented)")
            assert not grading_mode
            return
        if res != sk:
            print("your function simplify_skyline returned the wrong result on {}:".format(sky_red))
            print("it returned {}".format(res))
            print("instead of  {}".format(sk))
            assert not grading_mode
    print("done.")


def test_skyline(name, fn):
    print("testing {}...".format(name))
    # test run
    for rects, sky in [(rects1, sky1), (rects2, sky2), (rects3, sky3), (rects4, sky4), (rects5, sky5)]:
        res = fn(rects)
        if res is None:
            print("skipping {} (unimplemented)".format(name))
            assert not grading_mode
            return False
        if res != sky:
            print("your function {} returned the wrong result on {}:".format(name, rects))
            print("it returned {}".format(res))
            print("instead of  {}".format(sky))
            assert not grading_mode
            return False
    print("done.")
    return True

def test_skyline_naive():
    test_skyline("skyline_naive", sky.skyline_naive)

def test_skyline_merge():
    print("testing merge_skylines...")
    # test run
    for sky1, sky2, res in [(merge1_sky1, merge1_sky2, merge1_res), (merge2_sky1, merge2_sky2, merge2_res),
                            (merge3_sky1, merge3_sky2, merge3_res)]:
        tmp = sky.merge_skylines(sky1, sky2)
        if tmp is None:
            print("skipping merge_skylines (unimplemented)")
            assert not grading_mode
            return
        if res != tmp:
            print("your function merge_skylines returned the wrong result on:")
            print("sky1 = {}".format(sky1))
            print("sky2 = {}".format(sky2))
            print("it returned {}".format(tmp))
            print("instead of  {}".format(res))
            assert not grading_mode
            return False
    print("done.")

def gen_rand_rects(n):
    rects = []
    for _ in range(n):
        left = random.randrange(0, 500)
        right = left + random.randrange(1, 500)
        rects.append((left, random.randrange(0, 100), right))
    return rects

def test_skyline_dac():
    if not test_skyline("skyline_dac", sky.skyline_dac):
        return
    # tests random instances
    print("testing random instances...")
    for i in range(1, 11):
        rects = gen_rand_rects(10 * i)
        res_naive = sky.skyline_naive(rects)
        res_dac = sky.skyline_dac(rects)
        if res_naive != res_dac:
            print("your naive and DAC functions do not agree on:")
            print(rects)
            print("naive returned:")
            print(res_naive)
            print("DAC returned:")
            print(res_dac)
            assert not grading_mode
            return False
    print("testing complexity...")
    # test complexity
    for i in range(10, 500, 50):
        start = time.time()
        n = 1000 * i
        rects = gen_rand_rects(n)
        sky.skyline_dac(rects)
        end = time.time()
        print("time (n={}): {}".format(n, end - start))
        if end-start >= 10:
            print("Your DAC function took more than 10 seconds on an example of size {}, your complexity is probably not O(nlog(n))".format(n))
            assert not grading_mode
            return
    print("done.")

test_height_at()
test_simplify()
test_skyline_naive()
test_skyline_merge()
test_skyline_dac()


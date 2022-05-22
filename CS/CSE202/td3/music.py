# -*- coding: utf-8 -*-

import wav
import scipy.fftpack
import math

###################

def add_fixed_tone(samples):
    # we add a fixed tone A*sin(2*n*pi*f) where f=F/len(sampes)
    F = 2000 # frequency (Hz)
    A = 2 ** 13 # amplitude (between 0 and 2**15)
    return [samples[i] + A*math.sin(2*i/len(samples)*math.pi*F)
            for i in range(len(samples))]

def decompress_none(samples):
    return samples

print("processing samples 1.wav")
wav.process_file_by_chunk('sample 1.wav', 'sample 1 tone.wav', add_fixed_tone,
                          decompress_none, 512)


#Question 6, when we make F larger, I can hear a more and more harsh sound in the background.

###################

def compress_decimate2(samples):
    return samples[0::2]

def decompress_decimate2(samples):
    N = len(samples)
    L = [0 for i in range(N * 2)]
    for i in range(N):
        L[2 * i] = samples[i]
        L[2 * i + 1] = samples[i]
    return L

def compress_decimate4(samples):
    return samples[0::4]

def decompress_decimate4(samples):
    N = len(samples)
    L = [0 for i in range(N * 4)]
    for i in range(N):
        L[4 * i] = samples[i]
        L[4 * i + 1] = samples[i]
        L[4 * i + 2] = samples[i]
        L[4 * i + 3] = samples[i]
    return L

#
if False: # remove when you implemented the above functions
    print("processing samples 1.wav")
    wav.process_file_by_chunk('sample 1.wav', 'sample 1 half.wav', compress_decimate2,
                              decompress_decimate2, 512)
    wav.process_file_by_chunk('sample 1.wav', 'sample 1 four.wav', compress_decimate4,
                              decompress_decimate4, 512)

# DCT/IDCT
if False:
    array = [1,2,3,4,5,6,7,8]
    dct = scipy.fftpack.dct(array, norm='ortho') # use norm='ortho'
    print('DCT is {}'.format(dct))
    res = scipy.fftpack.idct(dct, norm='ortho') # use norm='ortho'
    print('IDCT(DCT) is {}'.format(res)) # should be array

def simplify(L, ratio=0.5):
    n = len(L)
    m = int(n * ratio)
    res = []
    v = [-abs(L[i][1]) for i in range(n)]
    v.sort()
    v = v[:m]
    for i in range(n):
        if (L[i][1] in v) or (-L[i][1] in v):
            res.append(L[i])
    return res

if False:
    print(simplify([(0,42),(2,13),(1,19),(3,25)], 0.5))

def compress_dct(samples):
    return scipy.fftpack.dct(samples, norm='ortho')

def decompress_idct(samples):
    return scipy.fftpack.idct(samples, norm='ortho')

def compress_dct_simplify(samples):
    L = compress_dct(samples)
    n = len(L)
    x = []
    for i in range(n):
        x.append((i, L[i]))
    S = simplify(x)
    return (len(samples), S)

if False:
    print(compress_dct_simplify([0,1,2,3,3,2,1,0]))

def decompress_idct_rebuild(samples):
    n, S = samples
    L = [0 for _ in range(n)]
    for i in S:
        index, v = i
        L[index] = v
    return decompress_idct(L)

if False:
    print(decompress_idct_rebuild((8, [(0, 4.242), (2, -3.154), (6, -0.224)])))

#
if True:
    print("processing samples 1.wav")
    wav.process_file_by_chunk('sample 1.wav', 'sample 1 out.wav', compress_dct_simplify,
                              decompress_idct_rebuild, 512)
    print("processing samples 2.wav")
    wav.process_file_by_chunk('sample 2.wav', 'sample 2 out.wav', compress_dct_simplify,
                          decompress_idct_rebuild, 512)

# Question 12.
# I hear the differeance from ratio=0.3, and when ratio = 0.15 I feel the quality degrades significantly.
# -*- coding: utf-8 -*-

import wave
import struct
from collections.abc import Iterable
from numbers import Number

# load a wave file, check that it is stereo, signed 16-bit samples at 44.1kHz
# returns the list of samples in a form of tuples (left,right)
def load_wav(filename):
    with wave.open(filename) as w:
        if w.getnchannels() != 2:
            raise Exception('this function only supports stereo files')
        if w.getsampwidth() != 2:
            raise Exception('this function only supports 16-bit sample width')
        if w.getcomptype() != 'NONE':
            raise Exception('this function does not support compression')
        if w.getframerate() != 44100:
            raise Exception('this function only supports 16-bit sample width')
        n = w.getnframes()
        frames = w.readframes(n)
        def get_sample(i):
            return struct.unpack('<hh', frames[4*i:4*i+4])
        return [get_sample(i) for i in range(n)]

# expects data in the form of list of tuples (left,right), the samples
# must be signed integers that can be written with 16-bits
def write_wav(filename, samples):
    with wave.open(filename, mode='wb') as w:
        w.setnchannels(2)
        w.setsampwidth(2)
        w.setframerate(44100)
        w.setnframes(len(samples))
        rawsamples = [struct.pack('<hh', l, r) for (l,r) in samples]
        w.writeframes(bytes([v for r in rawsamples for v in r ]))
        w.close()

# given an array, pad it with 0 so that its length is a multiple of n
def pad_to_multiple(data, n):
    if len(data) % n == 0:
        return data
    return data + [(0,0) for i in range(n - (len(data) % n))]

# takes a list of stereo samples and returns two list: one for the left, one for the right
def split_lr(samples):
    return ([l for (l,_) in samples], [r for (_,r) in samples])

# takes two list of samples (left and right) and merge them into a stereo list
def merge_lr(left, right):
    assert(len(left) == len(right))
    return [(left[i], right[i]) for i in range(len(left))]

def clamp_s16b(data):
    return max(min(0x7fff, data), -0x7fff-1)

def round_s16b(data):
    return [clamp_s16b(int(round(x.real))) for x in data]

def how_many_numbers(x):
    if isinstance(x, Number): return 1
    if isinstance(x, Iterable):
        return sum([how_many_numbers(y) for y in x])
    return 1

# process a file (in_file), apply some compression function, then apply
# some decompression function and then save the result to a file (out_file)
# the (de)compression functions are called on chunks of a fixed size, padded
# with zeroes if needed
# if split is True (default), then (de)compress are called independently on
# left and right channels, otherwise they are called on stereo
def process_file_by_chunk(in_file, out_file, compress, decompress, chunk_sz, split = True):
    # load file
    samples = load_wav(in_file)
    # pad to multiple of chunks
    sample_cnt = len(samples)
    samples = pad_to_multiple(samples, chunk_sz)
    # if split processing is set, create temporary lambdas for (de)compress
    if split:
        def apply_lr(fn, L):
            (l,r) = split_lr(L)
            return merge_lr(fn(l), fn(r))
        compress = lambda L,fn=compress: apply_lr(fn, L)
        decompress = lambda L,fn=decompress: apply_lr(fn, L)
    # process by chunk
    samples_out = [0] * len(samples)
    compressed_size = 0
    for i in range(0, len(samples), chunk_sz):
        chunk = samples[i:i+chunk_sz]
        # compress
        res = compress(chunk)
        compressed_size += how_many_numbers(res)
        # decompress
        samples_out[i:i+chunk_sz] = map(round_s16b, decompress(res))
    # stats
    print('File {} had {} samples, compressed to {} samples ({}%)'
          .format(in_file, len(samples) * 2, compressed_size,
                  compressed_size * 100 // len(samples) // 2))
    # remove extra samples added for padding
    samples_out = samples_out[0:sample_cnt]
    # write output
    write_wav(out_file, samples_out)
        
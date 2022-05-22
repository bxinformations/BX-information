import math
import hashlib

def bin_hash(x):
	hash_object = hashlib.sha1(str(x).encode('utf-8'))
	hexa=hash_object.hexdigest()
	bina=bin(int(hexa, 16))[2:].zfill(160)[:32]
	return bina
	
### CODE TO BE COMPLETED	
	
def cardinality(tab):
	m = {}
	res = 0
	for i in tab:
		m[i] = 1
	for i in tab:
		if (m[i] == 1):
			res += 1
		m[i] += 1
	return res
	
def bucket(bina,b): # returns the integer corresponding to the leftmost b bits in bina
	p = 1 << (b - 1)
	res = 0
	for i in range(b):
		res += int(bina[i]) * p
		p >>= 1
	return res
	
def zeros(bina,b): # return the largest l, called b-length of bina, such that all entries in bina[b:b+l] are zeros
	n = len(bina)
	ans = 0
	if (bina[b] == '1'):
		return 0
	for i in range(b, n):
		if (bina[i] == '0'):
			ans += 1
		else:
			break
	return ans
		
def sketch(L,b): # returns the array A of length 2**b, such that A[i] is 0 if the bucket of index i is empty, and otherwise A[i] is one plus the maximum value taken by the b-length over all elements in the bucket of index i  
	res = [0 for _ in range(1 << b)]
	for i in L:
		h = bin_hash(i)
		ind = bucket(h, b)
		z = zeros(h, b)
		res[ind] = max(res[ind], z + 1)
	return res
		
def constant(b): # function to compute the constant alpha(b), given
	if b==4: return 0.673
	elif b==5: return 0.697
	elif b==6: return 0.709
	else: return 0.7213/(1+1.079/2**b)
								

def loglog(L,b):
	m = 1 << b
	e = constant(b)
	e *= m * m
	su = 0
	s = sketch(L, b)
	for i in range(m):
		su += 1.0 / (1 << s[i])
	e /= su
	return e
	
def loglog_small_range_correction(L,b):
	e = loglog(L, b)
	m = 1 << b
	if (e > (5.0 * m) / 2.0):
		return e
	num = 0
	s = sketch(L, b)
	for i in range(m):
		if (s[i] == 0):
			num += 1
	if (num != 0):
		return m * math.log((1.0 * m) / num)
	else:
		return e

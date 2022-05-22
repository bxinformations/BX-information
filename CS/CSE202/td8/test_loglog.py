import random
import loglog as lo

card_sample=[([],0),(['a'],1),(['b','b'],1),(["aa","a","b","a","c","aa"],4)]

def test_cardinality():
	print("testing cardinality...")
	success=True
	for (x,h) in card_sample:
		res=lo.cardinality(x)
		if res is None:
			print("Unimplemented")
			print()
			return
		if res!=h:
			success=False
			print("your function cardinality returned the wrong result on {}:".format(x))
			print("it returned {} instead of {}".format(res, h))
	if success:
		print("Success.")
		print()	

bucket_sample=[("01",1,0),("11",1,1),("1000",3,4),("1101001",4,13),("010000",5,8)]

def test_bucket():
	print("testing bucket...")
	success=True
	for (bina,b,buc) in bucket_sample:
		res=lo.bucket(bina,b)
		if res is None:
			print("Unimplemented")
			print()
			return
		if res!=buc:
			success=False
			print("your function bucket returned the wrong result on ({},{}):".format(bina,b))
			print("it returned {} instead of {}".format(res, buc))
	if success:
		print("Success.")
		print()

zeros_sample=[("00",1,1),("01",1,0),("10000",2,3),("1010111",5,0),("100000011",3,4),("10000000000001",3,10)]

def test_zeros():
	print("testing zeros...")
	success=True
	for (bina,b,zer) in zeros_sample:
		res=lo.zeros(bina,b)
		if res is None:
			print("Unimplemented")
			print()
			return
		if res!=zer:
			success=False
			print("your function zeros returned the wrong result on ({},{}):".format(bina,b))
			print("it returned {} instead of {}".format(res, zer))
	if success:
		print("Success.")
		print()

tab1=["aa","a","b","a","c","aa"]
tab2=[]
for _ in range(3):
	tab2+=[str(i+200) for i in range(100)]
random.shuffle(tab2)

sketch_sample=[(tab1,1,[0,5]),(tab1,2,[0,0,4,1]),(tab2,1,[7,7]),(tab2,2,[6,5,6,7]),(tab2,8,[0, 2, 0, 0, 0, 2, 2, 0, 0, 1, 0, 2, 0, 0, 1, 0, 0, 6, 0, 2, 6, 0, 0, 0, 0, 4, 0, 2, 2, 0, 2, 0, 0, 0, 0, 0, 0, 3, 2, 0, 4, 3, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 2, 0, 2, 4, 0, 0, 5, 1, 3, 2, 0, 0, 0, 0, 1, 4, 0, 1, 0, 0, 0, 1, 1, 3, 4, 0, 0, 5, 0, 0, 1, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 9, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 5, 0, 3, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 7, 0, 1, 1, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 4, 0, 0, 3, 0, 1, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 6, 0, 0, 0, 0, 1, 0, 0, 6, 0, 0, 3, 0, 4, 0, 0, 0, 1, 1, 0, 0, 3, 0, 0, 0, 0, 1, 3, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 2, 0, 0, 0, 0, 2, 0, 2, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 2, 2, 1, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])]

def test_sketch():
	print("testing sketch...")
	success=True
	for (bina,b,ske) in sketch_sample:
		res=lo.sketch(bina,b)
		if res is None:
			print("Unimplemented")
			print()
			return
		if res!=ske:
			success=False
			print("your function sketch returned the wrong result on ({},{}):".format(bina,b))
			print("it returned {} instead of {}".format(res, ske))
	if success:
		print("Success.")
		print()
		
tab3=[]
for _ in range(4):
	tab3+=[str(i+30000) for i in range(100000)]	
random.shuffle(tab3)		
		
est2=[68,76,99,144]		
est3=[118820,106162,109907,100658]	
	
def test_loglog():
	print("testing loglog...")
	success=True
	if lo.loglog(["a"],1) is None:
		print("Unimplemented")
		print()
		return
	print("test on list of cardinality 100")
	for b in range(4,8):
		est=(int)(lo.loglog(tab2,b))
		print("your estimated cardinality (converted to int) for b={}: {}".format(b,est))
		correct=est2[b-4]
		if est!=correct:
			success=False
			print("Wrong result, the estimate here should be {}".format(correct))
		else:
			print("Correct")
	print()		
	print("test on list of cardinality 100000")
	for b in range(6,10):
		est=(int)(lo.loglog(tab3,b))
		print("your estimated cardinality (converted to int) for b={}: {}".format(b,est))
		correct=est3[b-6]
		if est!=correct:
			success=False
			print("Wrong result, the estimate here should be {}".format(correct))
		else:
			print("Correct")			
	if success:
		print("Success.")
		print()

est2_corr=[68,66,77,94]
est3_corr=[101036,99813,100053,99970]
		
def test_loglog_correction():
	print("testing loglog with correction...")
	success=True
	if lo.loglog_small_range_correction(["a"],1) is None:
		print("Unimplemented")
		print()
		return
	print("test on list of cardinality 100")
	for b in range(4,8):
		est=(int)(lo.loglog_small_range_correction(tab2,b))
		print("your estimated cardinality (converted to int) for b={}: {}".format(b,est))
		correct=est2_corr[b-4]
		if est!=correct:
			success=False
			print("Wrong result, the estimate here should be {}".format(correct))
		else:
			print("Correct")
	print()		
	print("test on list of cardinality 100000")		
	for b in range(15,19):
		est=(int)(lo.loglog_small_range_correction(tab3,b))
		print("your estimated cardinality (converted to int) for b={}: {}".format(b,est))
		correct=est3_corr[b-15]
		if est!=correct:
			success=False
			print("Wrong result, the estimate here should be {}".format(correct))
		else:
			print("Correct")					
	if success:
		print("Success.")
		print()		
					     
test_cardinality()	
test_bucket()	
test_zeros()	
test_sketch()
test_loglog()
test_loglog_correction()

import random

MAX = 200
n = 2*MAX

unique = random.sample(range(1,n), MAX)
unsorted = random.sample(range(1,n), MAX-5)
unsorted.append(1)
unsorted.append(2)
unsorted.append(3)
unsorted.append(3)
unsorted.append(199)


sorted_list = list(unsorted)
sorted_list.sort()

revsorted = list(sorted_list)
revsorted.reverse()

unsorted1 = list(sorted_list)
unsorted1.insert(0,unsorted1.pop())

unsorted2 = list(sorted_list)
unsorted2 = unsorted2[1:] + unsorted2[:1]


ones = [1 for x in range(MAX)]

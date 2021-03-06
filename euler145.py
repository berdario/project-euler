"""Some positive integers n have the property that the sum [ n + reverse(n) ] consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904 are reversible. Leading zeroes are not allowed in either n or reverse(n).

There are 120 reversible numbers below one-thousand.

How many reversible numbers are there below one-billion (10**9)?
"""

import math

class Myint(int):
	
	def __init__(self, *args, **kwargs):
		int.__init__(*args, **kwargs)
		self._len = int(math.floor(math.log10(self)+1))
		
	def __len__(self): 
		return self._len
		
	def __iter__(self):
		for i in xrange(len(self)):
			yield (self/(10**i))%10
		
	def reverse(self):
		l = len(self)
		for i in xrange(1,l+1):
			yield (self/(10**(l-i)))%10

def reversible(n):
	rn = n.reverse()
	rem = 0
	for d in n:
		newd = d + rn.next() + rem
		if (newd % 2):
			rem = newd // 10 
		else:
			return False
	return True


if __name__ == "__main__":
	count = 0
	for i in xrange(1,1000000000):
		if (i%10 != 0) and reversible(Myint(i)):
			count += 1
	print "Count: %d" % count

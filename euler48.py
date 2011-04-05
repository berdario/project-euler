from euler145 import Myint
def solve():
	a = 0L
	for i in xrange(1000):
		a += i**i
	a = Myint(a)
	b=[]
	for i,e in enumerate(a):
		if i>10: break
		b.append(e)
	return b

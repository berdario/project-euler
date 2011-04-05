all_sizes = (200,100,50,20,10,5,2,1)
sols = []
class solution(dict):
	def __missing__(self, value):
		#self[value]=0
		return 0

def calc(target=200, sol=(), el=None, sizes=all_sizes):
	if el:
		sol[el] += 1
	if not target:
		sols.append(sol)
		return
	for i,e in enumerate(sizes):
		if e<=target:
			calc(target-e, solution(sol), e, sizes[i:] )

if __name__ == "__main__":
	calc()
	print len(sols)


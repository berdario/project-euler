from math import log

try:
    from functools import lru_cache
except ImportError:
    lru_cache = None

## Naive implementation
#
# from itertools import count, takewhile
# from math import factorial

# f5 = lambda x: len(list(takewhile(lambda i:x%i==0, (5**i for i in count()) )))-1
# f5 = lru_cache(10000000000)(f5)

def factor5(n):
    result = 0
    while n%5==0:
        n //= 5
        result += 1
    return result

def factor3(n):
    result = 0
    while n%3==0:
        n //= 3
        result += 1
    return result

f5s = {}

def ff5(n):
    global f5s
    result = 0
    while n > 4:
        result += factor5(n)
        previous_ff5 = f5s.get(n-1, None)
        if previous_ff5:
            result += previous_ff5
            f5s[n]=result
            return result
        n -= 1
    return result

def sff5(n):
    result = 0
    while n > 4:
        l = log(n, 5)
        if l == int(l):
            return result + sum(5**i for i in range(int(l)))
        else:
            result += factor5(n)
            n -= 1
    return result

def ssff5(n):
    global f5s
    result = 0
    while n > 4:
        l = log(n, 5)
        if l == int(l):
            return result + sum(5**i for i in range(int(l)))
        else:
            result += factor5(n)
            previous_ff5 = f5s.get(n-1, None)
            if previous_ff5:
                result += previous_ff5
                f5s[n]=result
                return result
            n -= 1
    return result


if lru_cache:
    ff5 = lru_cache(10000000000)(ff5)

t = lambda i: ff5(2*i-1) < 2*ff5(i)

if __name__ == "__main__":
    print(len([1 for i in range(1,4001) if t(i)]))


#f5(factorial(5*5*5*25)) == 5*5*(5*f5(factorial(25))+1)

def ff(n):
    if n<5:
        return 0
    elif n<10:
        return 1
    elif n<25:
        return 2
    else:
        e = log(n,5)
        d,r = n//25, n%25
        if r==0:
            return d*2
        else:
            return d*2+ff(r)
        
        

#! /usr/bin/env scala
!#

import math.{log10,floor,pow}

class MyInt(private var n:Int) extends java.lang.Number with Iterable[Int]{

	def length() = floor(log10(n)).toInt

	override def iterator(): Iterator[Int] = digits().iterator

	def digits(n:Int = n):Stream[Int] = {
		n match {
			case 0 => Stream.empty
			case _ => Stream.cons(n%10, digits(n/10))
		}
	}
	
	def reverse(n:Int = n, numDigits:Int = length):Stream[Int] = {
		numDigits >= 0 match{
			case true => Stream.cons((n / pow(10, numDigits).toInt) % 10, reverse(n, numDigits-1))
			case false => Stream.empty
		}
	}
	
	def intValue = n
	def doubleValue = n
	def floatValue = n
	def longValue = n
}

def reversible(d: Stream[Int], dr: Stream[Int], rem: Int = 0): Boolean = {
	(d, dr) match{
		case (Stream(), Stream()) => true
		case (dh#::dt, drh#::drt) => {
			var newd = dh+drh+rem
			newd%2!=0 && reversible(dt, drt, newd/10)
		}
	}
}

def solve(n: Int = 1000): Int = {
	var count = 0
	var j: MyInt = new MyInt(0)
	for (i <- 1 to n if i%10!=0){
		j = new MyInt(i)
		if (reversible(j.digits(), j.reverse())){
			count+=1
		}
	}
	return count
}


var a:Long=0
var b:Long=0
a=System.currentTimeMillis()
solve(pow(10,7).toInt)
b=System.currentTimeMillis()
print((b-a)/1000.0)


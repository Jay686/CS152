val fibs = List(1, 1, 2, 3, 5, 8, 13)

// 1.
def cubes(n: Int) = n * n * n
def isOdd(n: Int) = n % 2 != 0

// Iterative version
def sumOfCubesIte(vals: List[Int]) = {
  var result = 0
  for(v <- vals if isOdd(v)) result += cubes(v)
  result
}

// Recursive version
def sumOfCubesRec(vals: List[Int]): Int =
  if(vals == Nil) 0
  else if(isOdd(vals.head))
    cubes(vals.head) + sumOfCubesRec(vals.tail)
  else sumOfCubesRec(vals.tail)

// Tail-recursive version
def sumOfCubesTailRec(vals: List[Int], result: Int = 0): Int = {
  if (vals == Nil) result
  else if(isOdd(vals.head))
    sumOfCubesTailRec(vals.tail, result + cubes(vals.head))
  else sumOfCubesTailRec(vals.tail, result)
}

// Pipeline version
def sumOfCubesPipeLine(vals: List[Int]) =
  vals.filter(isOdd).map(cubes).reduce(_ + _)

sumOfCubesIte(fibs)
sumOfCubesRec(fibs)
sumOfCubesTailRec(fibs)
sumOfCubesPipeLine(fibs)

/*
val res0: Int = 2351
val res1: Int = 2351
val res2: Int = 2351
val res3: Int = 2351
 */

// 2.

// Iterative version
def sumOfSumsIte(vals: List[List[Int]]) = {
  var result = 0
  for(v <- vals)
    for(u <- v) result += u
  result
}

// Recursive version
def sumOfSumsRec(vals: List[List[Int]]): Int =
  if(vals == Nil) 0
  else if(vals.head != Nil)
    vals.head.sum + sumOfSumsRec(vals.tail)
  else sumOfSumsRec(vals.tail)

// Tail-recursive version
def sumOfSumsTailRec(vals: List[List[Int]], result: Int = 0): Int = {
  if (vals == Nil) result
  else if(vals.head != Nil)
    sumOfSumsTailRec(vals.tail, result + vals.head.sum)
  else sumOfSumsTailRec(vals.tail, result)
}

// Pipeline version
def sumOfSumsPipeLine(vals: List[List[Int]]) =
  vals.map(_.reduce(_ + _)).reduce(_ + _)

val a = List(List(100, 95, 86, 42), List(35, 73, 80, 43), List(66, 80, 23, 55))

sumOfSumsIte(a)
sumOfSumsRec(a)
sumOfSumsTailRec(a)
sumOfSumsPipeLine(a)

/*
val res4: Int = 778
val res5: Int = 778
val res6: Int = 778
val res7: Int = 778
 */

// 6.
// Iterative version
def countIte[T](vals: List[T], f: T => Boolean) = {
  var result = 0
  for(v <- vals if f(v)) result += 1
  result
}

// Recursive version
def countRec[T](vals: List[T], f: T => Boolean): Int =
  if(vals == Nil) 0
  else if(f(vals.head))
    1 + countRec(vals.tail, f)
  else countRec(vals.tail, f)

// Tail-recursive version
def countTailRec[T](vals: List[T], f: T => Boolean, result: Int = 0): Int = {
  if (vals == Nil) result
  else if(f(vals.head))
    countTailRec(vals.tail, f, result + 1)
  else countTailRec(vals.tail, f, result)
}

// Pipeline version
def countPipeLine[T](vals: List[T], f: T => Boolean) =
  vals.filter(f).map(_ => 1).reduce(_ + _)

countIte(fibs, isOdd)
countRec(fibs, isOdd)
countTailRec(fibs, isOdd)
countPipeLine(fibs, isOdd)

/*
val res8: Int = 5
val res9: Int = 5
val res10: Int = 5
val res11: Int = 5
 */

// 7.
// Iterative version
def testIte[T](vals: List[T], f: T => Boolean) = {
  var result = true
  for(v <- vals if !f(v)) result = false
  result
}

// Recursive version
def testRec[T](vals: List[T], f: T => Boolean): Boolean =
  if(vals == Nil) true
  else f(vals.head) && testRec(vals.tail, f)

// Tail-recursive version
def testTailRec[T](vals: List[T], f: T => Boolean): Boolean =
  if(vals == Nil) true
  else if(f(vals.head)) testTailRec(vals.tail, f)
  else false

// Pipeline version
def testPipeLine[T](vals: List[T], f: T => Boolean) =
  vals.map(f).reduce(_ && _)

val f = List(1, 3, 5)

testIte(f, isOdd)
testRec(f, isOdd)
testTailRec(f, isOdd)
testPipeLine(f, isOdd)
testIte(fibs, isOdd)
testRec(fibs, isOdd)
testTailRec(fibs, isOdd)
testPipeLine(fibs, isOdd)

/*
val res12: Boolean = true
val res13: Boolean = true
val res14: Boolean = true
val res15: Boolean = true
val res16: Boolean = false
val res17: Boolean = false
val res18: Boolean = false
val res19: Boolean = false
 */

// 13.
def sFrom(a: Int, f: Int => Int): Stream[Int] = a #:: sFrom(f(a), f)
val s1 = sFrom(1: Int,(b: Int) => b)
val s2 = sFrom(0: Int,(b: Int) => b + 1)
val s3 = sFrom(0: Int,(b: Int) => b + 2)
val s4 = 0 #:: sFrom(1: Int, (b: Int) => (math.sqrt(b).toInt + 1) * (math.sqrt(b).toInt + 1))

s1.take(8).toList
s2.take(8).toList
s3.take(8).toList
s4.take(8).toList

/*
val res20: List[Int] = List(1, 1, 1, 1, 1, 1, 1, 1)
val res21: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7)
val res22: List[Int] = List(0, 2, 4, 6, 8, 10, 12, 14)
val res23: List[Int] = List(0, 1, 4, 9, 16, 25, 36, 49)
 */

// 15.
def spellCheck(doc: List[String], dictionary: List[String]): List[String] =
  doc.map((a: String) => if(dictionary.contains(a)) null else a).filter(_ != null)

spellCheck(List("car", "cat", "one"), List("one", "two", "cat"))

/*
val res24: List[String] = List(car)
 */

// 16.
def evalMono(mono: (Double, Double), x: Double) = {
  def exp(y: Double, n: Double, r: Double = 1): Double =
    if(n == 0) r else exp(y, n - 1, y * r)
  mono._1 * exp(x, mono._2)
}

def evalPoly(poly: List[(Double, Double)], x: Double) =
  poly.map(evalMono(_, x)).reduce(_ + _)

evalPoly(List((3.0, 2.0), (-5.0, 1.0)), 3.0)
evalPoly(List((3.0, 3.0), (-5.0, 2.0), (-5.0, 0.0)), 2.0)

/*
val res24: Double = 12.0
val res25: Double = -1.0
 */
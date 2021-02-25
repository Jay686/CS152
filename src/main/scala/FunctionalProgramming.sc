// 1.
def compose[A, B ,C](g: A => B, f: B => C) = (x: A) => f(g(x))

// 2.
def id[T](x: T) = x

def selfIter[T](f: T => T, n: Int): T => T = (x: T) =>
  if(n == 0) id(x)
  else if(n == 1) f(x)
  else compose(selfIter(f, n - 1), f)(x)

def inc(x: Double) = x + 1
def double(x: Double) = 2 * x

selfIter(inc, 0)(1)
selfIter(double, 0)(2)
selfIter(inc, 1)(1)
selfIter(double, 1)(2)
selfIter(inc, 9)(1)
selfIter(double, 5)(2)

/*
val res0: Double = 1.0
val res1: Double = 2.0
val res2: Double = 2.0
val res3: Double = 4.0
val res4: Double = 10.0
val res5: Double = 64.0
 */

// 3.
def countPass[T](inputs: Array[T], f: T => Boolean): Int = {
  var result = 0
  for(x <- inputs) if(f(x)) result +=1
  result
}

countPass(Array(1, 2, 3, 4, 5, 6), (x: Int) => x % 2 == 0)
countPass(Array(1, 1, 1, 1, 5, 6), (x: Int) => x % 2 == 0)

/*
val res6: Int = 3
val res7: Int = 1
 */

// 4.
def recur(baseVal: Int, combiner: (Int, Int) => Int): Int => Int = {
  def f(n: Int): Int = if(n == 0) baseVal else combiner(n, f(n-1))
  f _
}

val fac = recur(1, (n: Int, m: Int) => n * m)

fac(5)
fac(10)

/*
val res8: Int = 120
val res9: Int = 3628800
 */

// 5.
def deOptionize[T, S](f: T => Option[S]) =
  (x: T) => f(x) match {
    case None => throw new Exception("error!")
    case Some(n) => n
  }

def parseDigits(digits: String): Option[Int] =
  if (digits.matches("[0-9]+")) Some(digits.toInt) else None

deOptionize(parseDigits)("1")
deOptionize(parseDigits)("90")
//deOptionize(parseDigits)("one")

/*
val res10: Int = 1
val res12: Int = 90
java.lang.Exception: error!
  at $anonfun$deOptionize$1(<console>:4)
  ... 40 elided
 */

// 6.
def makeIter[T](init: T, f: T => T, n: Int): T =
  if(n == 0) init else makeIter(f(init), f, n - 1)

def sqaure(x: Double) = x * x

def iterSquare(init: Double, n: Int) =
  makeIter(init, sqaure, n)

iterSquare(2.0, 3)
iterSquare(3.0, 3)

/*
val res12: Double = 256.0
val res13: Double = 6561.0
 */

// 7.
def unitTest[T, S](f: T => S, inputs: Array[(T, S)]) = {
  var result = 0
  for(x <- inputs) if(f(x._1) != x._2) result += 1
  result
}

def cube(n: Int) = n * n * n
unitTest(cube, Array((1, 1), (2, 8), (3, 9), (4, 64), (5, 124)))

/*
val res14: Int = 2
 */

// 8.1
def controlLoop[S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S, Int) => S): S =
  if(halt(state, cycle)) state
  else controlLoop(update(state,cycle), cycle + 1, halt, update)

// 8.2
def population(limit: Int) = {
  def halt(currPop: Int, weeks: Int) = currPop >= limit
  def nextPop(currPop: Int, weeks: Int) = currPop * 2
  controlLoop(1, 0, halt, nextPop)
}

population(100000)

/*
val res15: Int = 131072
 */

// 8.3
def solve(f: Double => Double) = {
  val DELTA = 1e-10
  def halt(guess: Double, time: Int) = math.abs(f(guess)) <= DELTA
  def d(f: Double => Double) = (x: Double) => (f(x + DELTA) - f(x)) / DELTA
  def update(guess: Double, time: Int) = guess - f(guess) / d(f)(guess)
  controlLoop(1.0, 0, halt, update)
}

// 8.4
def squareRoot(x: Double) = solve((r: Double) => r * r - x)

squareRoot(0.2)
squareRoot(2.0)
squareRoot(3.0)
squareRoot(9.0)

/*
val res16: Double = 0.44721359550000167
val res17: Double = 1.414213562374277
val res18: Double = 1.7320508075688794
val res19: Double = 3.0
 */

// 8.5
def cubeRoot(x: Double) = solve((r: Double) => r * r * r - x)

cubeRoot(0.2)
cubeRoot(2.0)
cubeRoot(3.0)
cubeRoot(729.0)

/*
val res20: Double = 0.5848035476552512
val res21: Double = 1.2599210498948732
val res22: Double = 1.4422495703072642
val res23: Double = 9.0
 */

// 8.6
def nthRoot(x: Double, n: Int) = {
  def f(i: Double, k: Int, m: Double = 1): Double =
    if(k == 0) m else f(i, k - 1, i * m)
  solve((r: Double) => f(r, n) - x)
}

nthRoot(729.0, 6)

/*
val res24: Double = 2.999999999999998
 */

// 8.7
def futureValue(periods: Int) = {
  def end(currVal: Double, age: Int) = age == periods
  val rate = 1.0 / periods
  def payInterest(currVal: Double, age: Int) = currVal + rate * currVal
  controlLoop(1.0, 0, end, payInterest)
}

futureValue(12)
futureValue(365)
futureValue(365 * 24)
futureValue(365 * 24 * 60 * 60)

/*
val res25: Double = 2.613035290224677
val res26: Double = 2.714567482021875
val res28: Double = 2.718126691620467
val res29: Double = 2.7182817853606362
 */

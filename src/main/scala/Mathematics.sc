import scala.annotation.tailrec
import scala.util.Random
// Mathematics

// 1. Polynomials
object poly {

  def roots(p: (Double, Double, Double)): Option[(Double, Double)] = {
    val delta = p._2 * p._2 - 4 * p._1 * p._3
    // = None if p has no real roots
    if(delta < 0) None
    // = Some((r1, r2)) where p(r1) == p(r2) == 0
    else Some((-p._2 + math.sqrt(delta)) / 2 / p._1, (-p._2 - math.sqrt(delta)) / 2 / p._1)
  }

  def deriv(p: (Double, Double, Double)): (Double, Double, Double) =
  // = derivative of p (which should be degree 1
  (0.0, 2 * p._1, p._2)

  def eval(a: Double, p: (Double, Double, Double)): Double =
  // = p(a)
  p._1 * a * a + p._2 * a + p._3
}

val p = (3.0, 9.0, -30.0) // = (3x - 6) * (x + 5)

println("eval(6, p) = " + poly.eval(6, p))
println("eval(2, p) = " + poly.eval(2, p))
println("eval(-5, p) = " + poly.eval(-5, p))
println("roots(p) = " + poly.roots(p))
println("deriv(p) = " + poly.deriv(p))
println("deriv2(p) = " + poly.deriv(poly.deriv(p)))

/*
eval(6, p) = 132.0
eval(2, p) = 0.0
eval(-5, p) = 0.0
roots(p) = Some((2.0,-5.0))
deriv(p) = (0.0,6.0,9.0)
deriv2(p) = (0.0,0.0,6.0)
 */

// 2. Linear Algebra
object vector {

  def sum(v1: (Double, Double, Double), v2: (Double, Double, Double)): (Double, Double, Double) =
  // v1 + v2
    (v1._1 + v2._1, v1._2 + v2._2, v1._3 + v2._3)

  def mul(a: Double,v: (Double, Double, Double)): (Double, Double, Double) =
  // = a * v
    (a * v._1, a * v._2, a * v._3)

  def dot(v1: (Double, Double, Double), v2: (Double, Double, Double)): Double =
  // = v1 * v2
    v1._1 * v2._1 + v1._2 * v2._2 + v1._3 * v2._3

  def length(v: (Double, Double, Double)): Double =
  // = |v|
  math.sqrt(v._1 * v._1 + v._2 * v._2 + v._3 * v._3)

  def theta(v1: (Double, Double, Double), v2: (Double, Double, Double)): Double =
  // = angle (in radians) between v1 and v2
    math.acos(dot(v1, v2) / (length(v1) * length(v2)))
}

val v1 = (2.0, 2.0, 2.0)
val v2 = (1.0, 0.0, 0.0)
val v3 = (0.0, 1.0, 0.0)

println("sum(v3, v2) = " + vector.sum(v3, v2))
println("mul(3, v1) = " + vector.mul(3, v1))
println("dot(v1, v2) = " + vector.dot(v1, v2))
println("dot(v2, v3) = " + vector.dot(v2, v3))
println("dot(v1, v1) = " + vector.dot(v1, v1))
println("length(v1) = " + vector.length(v1))
println("length(v2) = " + vector.length(v2))
println("theta(v1, v2) = " + vector.theta(v1, v2))
println("theta(v3, v2) = " + vector.theta(v3, v2))
println("pi/2 = " + Math.PI/2)

/*
sum(v3, v2) = (1.0,1.0,0.0)
mul(3, v1) = (6.0,6.0,6.0)
dot(v1, v2) = 2.0
dot(v2, v3) = 0.0
dot(v1, v1) = 12.0
length(v1) = 3.4641016151377544
length(v2) = 1.0
theta(v1, v2) = 0.9553166181245092
theta(v3, v2) = 1.5707963267948966
pi/2 = 1.5707963267948966
 */

// 3. Number Theory
object arithmetic {

  // = None if n < 0
  // = largest int m such that m * m <= n
  def sqrt(n: Int): Option[Int] = {
    if(n < 0) None
    else Some(math.sqrt(n).floor.toInt)
  }

  // = None if N <= 0
  // = largest m such that 2^m <= n
  def log(n: Int): Option[Int] = {
    if(n <= 0) None
    else Some((math.log(n)/math.log(2)).floor.toInt)
  }

  // = true if n has no divisors > 1
  def isPrime(n: Int): Option[Boolean] = {
    if(n < 0) None
    else if(n < 2) Some(false)
    else Some((2 until math.sqrt(n).floor.toInt) forall(n % _ != 0))
  }

  // = None if n or m < 0
  // = gcd(m, n) if n < m
  // = largest k dividing both n and m
  def gcd(n: Int, m: Int): Option[Int] = {
    if(n < 0) None
    else if(m < 0) None
    else if(n < m) gcd(m, n)
    else {
      var a = n
      var b = m
      while(a != 0) {
        var temp = a
        a = b % a
        b = temp
      }
      Some(b)
    }
  }

  // = None if n < 0 or m < 0
  // = smallest k such that n a,d m divide k
  def lcm(n: Int, m: Int): Option[Int] = {
    if(n < 0) None
    else if(m < 0) None
    else Some((n * m) / gcd(n, m).get)
  }

  // = None if n < 0
  // = # of k <= n such that gcd(k, n) = 1
  def phi(n: Int): Option[Int] = {
    var k = 0
    if(n < 0) None
    else {
      for(i <- 1 until n) if(gcd(i, n) == Option(1)) k += 1
      Some(k)
    }
  }

}

println("gcd(15, 12) = " + arithmetic.gcd(15, 12))
println("gcd(12, 15) = " + arithmetic.gcd(12, 15))
println("lcm(15, 12) = " + arithmetic.lcm(15, 12))
println("gcd(13, 12) = " + arithmetic.gcd(13, 12))
println("gcd(-13, 12) = " + arithmetic.gcd(-13, 12))
println("phi(9)= " + arithmetic.phi(9))
println("sqrt(49) = " + arithmetic.sqrt(49))
println("sqrt(37) = " + arithmetic.sqrt(37))
println("sqrt(35) = " + arithmetic.sqrt(35))
println("log(64) = " + arithmetic.log(64))
println("log(130) = " + arithmetic.log(130))
println("log(9) = " + arithmetic.log(9))
println("log(0) = " + arithmetic.log(0))
println("isPrime(23) = " + arithmetic.isPrime(23))
println("isPrime(59) = " + arithmetic.isPrime(59))
println("isPrime(75) = " + arithmetic.isPrime(75))

/*
gcd(15, 12) = Some(3)
gcd(12, 15) = Some(3)
lcm(15, 12) = Some(60)
gcd(13, 12) = Some(1)
gcd(-13, 12) = None
phi(9)= Some(6)
sqrt(49) = Some(7)
sqrt(37) = Some(6)
sqrt(35) = Some(5)
log(64) = Some(6)
log(130) = Some(7)
log(9) = Some(3)
log(0) = None
isPrime(23) = Some(true)
isPrime(59) = Some(true)
isPrime(75) = Some(false)
 */

// 4.
def rollDice =
  ((math.random() * 100 % 6 + 1).toInt, (math.random() * 100 % 6 + 1).toInt)

rollDice
rollDice
rollDice

/*
val res32: (Int, Int) = (2,2)
val res33: (Int, Int) = (6,1)
val res34: (Int, Int) = (5,6)
 */


// Solutions
/*
  def sqrt(n: Int) = {
    if (n < 0) None
    else {
      var result = 0
      for(i <- 0 until n/2 if i * i <= n) {
        result = i
      }
      Some(result)
    }
  }

  def log(n: Int) = {
    if (n <= 0) None
    else {
      var result = 1
      var pow = 1
      while(2 * pow <= n) {
        result += 1
        pow = 2 * pow
      }
      Some(result)
    }
  }

  def isPrime(n: Int) = {
    if (n < 0) None
    else if (n < 2) Some(false)
    else {
      var result = true
      for(i <- 2 until sqrt(n).get if result) result = (n % i != 0)
      Some(result)
    }
  }

  def gcd(n: Int, m: Int): Option[Int] = {
    if (m < 0 || n < 0) None
    else if (m == 0 || n == 0) Some(0)
    else {
      var result = 1
      for(i <- 2 to math.min(n, m) if m % i == 0 && n % i == 0) result = i
      Some(result)
    }
  }

  def lcm(n: Int, m: Int) = {
    if (m < 0 || n < 0) None
    else if (m == 0 || n == 0) Some(0)
    else {
      var result = 1
      var done = false
      for(i <- 2 to m * n if !done && i % m == 0 && i % n == 0) {
        result = i
        done = true
      }
      Some(result)
    }
  }

  def phi(n: Int) = {
    if (n < 0) None
    var result = 0
    for(i <- 1 to n)
      if (gcd(n, i).get == 1) result += 1;
    Some(result)
  }
 */

// Poly
/*
object poly {

  def roots(p: (Double, Double, Double)): Option[(Double, Double)] = {
    val a = p._1
    val b = p._2
    val c = p._3
    val disc = b * b - 4 * a * c
    if (disc < 0) None
    else {
      val disc2 = math.sqrt(disc)
      val root1 = (-b + disc2)/ (2 * a)
      val root2 = (-b - disc2)/ (2 * a)
      Some((root1, root2))
    }
  }

  def deriv(p: (Double, Double, Double)): (Double, Double, Double) =
    (0, 2 * p._1, p._2)

  def eval(a: Double, p: (Double, Double, Double)): Double =
    p._1 * a * a + p._2 * a + p._3
}
 */

// Vector
/*
object vector {

  def sum(v1: (Double, Double, Double), v2: (Double, Double, Double)): (Double, Double, Double) =
    (v1._1 + v2._1, v1._2 + v2._2, v1._3 + v2._3)

  def mul(a: Double,v: (Double, Double, Double)): (Double, Double, Double) =
    (a * v._1, a * v._2, a * v._3)

  def dot(v1: (Double, Double, Double), v2: (Double, Double, Double)): Double =
    v1._1 * v2._1 + v1._2 * v2._2 + v1._3 * v2._3

  def length(v: (Double, Double, Double)): Double =
    math.sqrt(dot(v, v))

  def theta(v1: (Double, Double, Double), v2: (Double, Double, Double)): Double =
    math.acos(dot(v1, v2)/length(v1) * length(v2))

}
 */

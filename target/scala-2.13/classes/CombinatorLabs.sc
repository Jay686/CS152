def apply(f: String=>String) = f("Mississippi")
apply((x: String)=>x+x.reverse)

def shift(f: Double => Double, a: Double) = (x: Double) => f(x + a)
val mystery = shift((x: Double) => x * x * x, 3.0)
mystery(2) // =125
/*
def shift(f: Double => Double, a: Double) = {
  def g(x: Double) = f(x + a)
  g _
}*/

def amplify(f: Double => Double, a: Double) = (x: Double) => a * f(x)
val mystery = amplify((x: Double) => x * x * x, 3)
mystery(2)

def branch(f: Double => Double, g: Double => Double) = (x: Double) => if (0 <= x) f(x) else g(x)
//val mystery = branch((x: Double) => math.sin(x), (x: Double) => math.cos(x))
val mystery = branch(math.sin _, math.cos _)
mystery(math.Pi / 4)
mystery(-math.Pi / 4)

def countNone[T, S](inputs: Array[T], f: T => Option[S]): Int = {
  var result = 0
  for(x <- inputs) if(f(x) == None) result +=1
  result
}

def isPal(s: String) = if(s == s.reverse) Some(s) else None
def isEven(x: Int) = if(x % 2 == 0) Some(x) else None



def compose(f: Double => Double, g: Double => Double) = (x: Double) => f(g(x))
//selfIter(f, 4) = compose(f, compose(f, compose(f, f)))

def tri(n: Int): Int = if(n == 0) 0 else n + tri(n - 1)
def fact(n: Int): Int = if(n == 0) 1 else n * fact(n - 1)

def controlLoop[S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S, Int) => S): S = {
  if(halt(state, cycle)) state
  else controlLoop(update(state,cycle), cycle + 1, halt, update)
}
//def solve(f: Double => Double) = {
//  val DELTA = 1e-10
//  def goodEnuf(guess: Double, time: Int) = math.abs(f(guess)) <= DELTA
//  val df = deriv(f)
//  def deriv(f: Double => Double) = {
//    def fPrime(x: Double) = (f(x+DELTA) - f(x)) / DELTA
//    fPrime _
//  }
//  def improve(guess: Double, time: Int) = guess - f(guess) / df(guess)
//  controlLoop(1.0, 0, goodEnuf, improve)
//}

// invest $1 for 1 year at 100% interest, compounded monthly, weekly, daily, etc.
def futureValue(periods: Int) = {
  def end(currVal: Double, age: Int) = age == periods
  val rate = 1.0 / periods
  def payInterest(currVal: Double, age: Int) = currVal + rate * currVal
  controlLoop(1.0, 0, end, payInterest)
}
futureValue(12)
futureValue(52)
futureValue(365)

//4
def makeRecur(init, combine): Unit = {
  def foo(n: Int): Int = if(n == 0) init else combine(n, foo(n-1))
  foo _
}

def makeIter(f, int) = {
  def iterf
}

def parseDigits(digits: String) =
  if(digits.matches("[0-9]+")) Some(digits.toInt) else None

def parseDigits2(digits: String) =
  parseDigits(digits) match {
    case Some(n) => n
    case None => throw new Exception("Invalid Input")
  }

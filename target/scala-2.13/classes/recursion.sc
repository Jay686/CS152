import scala.annotation.tailrec
import scala.io.StdIn.readLine

def inc(n: Int) = n + 1
def dec(n: Int) = n - 1
def isZero(n: Int) = n == 0

/*In the following problems your solutions may assume
that all inputs are non-negative integers
(these are sometimes called the natural numbers).*/

// 1. add(n: Int, m: Int) = n + m
def add(n:Int, m: Int): Int =
  if(isZero(n)) m else inc(add(dec(n), m))

add(1, 2)
add(3, 6)

/*
val res0: Int = 3
val res1: Int = 9
 */

// 2. mul(n: Int, m: Int) = n * m
def mul(n: Int, m: Int): Int =
  if(isZero(n)) 0 else add(m, mul(dec(n), m))

mul(2, 3)
mul(9, 18)

/*
val res2: Int = 6
val res3: Int = 162
 */

// 3. exp(m: Int) = pow(2, m) = 2^m
def exp(m: Int): Int =
  if(isZero(m)) 1 else mul(2, exp(dec(m)))

exp(10)
exp(12)

/*
val res4: Int = 1024
val res5: Int = 4096
 */

// 4. hyperExp(n: Int) = exp(exp(... (exp(0)) ...)) n-times
def hyperExp(n: Int): Int =
  if(isZero(n)) 0 else exp(hyperExp(dec(n)))

hyperExp(0)
hyperExp(1)
hyperExp(2)
hyperExp(3)
hyperExp(4)

/*
val res6: Int = 0
val res7: Int = 1
val res8: Int = 2
val res9: Int = 4
val res10: Int = 16
 */

// 5.
@tailrec
def add1(n: Int, m: Int): Int =
  if(isZero(n)) m else add1(dec(n), inc(m))

add1(1, 2)
add1(3, 6)

/*
val res11: Int = 3
val res12: Int = 9
 */

@tailrec
def mul1(n: Int, m: Int, r: Int = 0): Int =
  if(isZero(n)) r else mul1(dec(n), m, add1(m, r))

mul1(2, 3)
mul1(9, 18)

/*
val res13: Int = 6
val res14: Int = 162
 */

@tailrec
def exp1(m: Int, r: Int = 2): Int =
  if(isZero(m)) 1 else if(isZero(dec(m))) r
  else exp1(dec(m), mul1(2, r))

exp1(10)
exp1(12)

/*
val res15: Int = 1024
val res16: Int = 4096
 */

@tailrec
def hyperExp1(n: Int, r: Int = 1): Int =
  if(isZero(n)) 0 else if(isZero(dec(n))) r
  else hyperExp1(dec(n), exp1(r))

hyperExp1(0)
hyperExp1(1)
hyperExp1(2)
hyperExp1(3)
hyperExp1(4)
hyperExp1(5)

/*
val res17: Int = 0
val res18: Int = 1
val res19: Int = 2
val res20: Int = 4
val res21: Int = 16
val res22: Int = 65536
 */

// 9.
def fib(n: Int): Int =
  if(isZero(n)) 0 else if(isZero(dec(n))) 1
  else add(fib(dec(n)), fib(dec(dec(n))))

@tailrec
def fib1(n: Int, r1: Int = 0, r2: Int = 1): Int =
  if(isZero(n)) r1 else fib1(dec(n), r2, add1(r1, r2))

fib(1)
fib1(1)
fib(2)
fib1(2)
fib(3)
fib1(3)
fib(4)
fib1(4)
fib(5)
fib1(5)
fib1(22)

/*
val res23: Int = 1
val res24: Int = 1
val res25: Int = 1
val res26: Int = 1
val res27: Int = 2
val res28: Int = 2
val res29: Int = 3
val res30: Int = 3
val res31: Int = 5
val res32: Int = 5
val res33: Int = 17711
 */



// 10. choose(n, m) = # of ways to choose m things from n. n, m >= 0
def choose(n: Int, m: Int): Int =
  if(isZero(m)) 1 else if(m > n) 0
  else add(choose(dec(n), dec(m)), choose(dec(n), m))

choose(20, 8)
choose(8, 7)
choose(10, 6)

/*
val res34: Int = 125970
val res35: Int = 8
val res36: Int = 210
 */

object Calculator {

  def repl {
    var more = true
    while(more) {
      try {
        var cmmd = readLine("-> ").split("\\s+")
        if (cmmd(0).equals("quit")) {
          more = false
        } else {
          if (cmmd.length != 3) {
            throw new Exception("syntax = NUMBER OPERATOR NUMBER")
          }
          var arg1 = cmmd(0).toDouble
          var arg2 = cmmd(2).toDouble
          if (cmmd(1) == "+") {
            println("result = " + (arg1 + arg2))
          } else if (cmmd(1) == "*") {
            println("result = " + (arg1 * arg2))
          } else if (cmmd(1) == "-") {
            println("result = " + (arg1 - arg2))
          } else if (cmmd(1) == "/") {
            println("result = " + (arg1 / arg2))
          } else {
            throw new Exception("unrecognized operator: " + cmmd(1))
          }
        }
      } catch {
        case e:Exception => println(e)
      }
    }
    println("bye")
  }
  def main(args: Array[String]): Unit = {repl}

}

//Calculator.repl
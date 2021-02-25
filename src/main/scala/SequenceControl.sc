//Sequence Control

// 1. "Number Kingdoms"
def kingdom(n: Int): Int =
  if(n <= 10) 3 else if(n % 100 == 0) 2 else if(n % 2 ==0) 1 else 4

kingdom(0)
kingdom(6)
kingdom(10)
kingdom(13)
kingdom(86)
kingdom(202)
kingdom(300)
kingdom(953)

/*
val res0: Int = 3
val res1: Int = 3
val res2: Int = 3
val res3: Int = 4
val res4: Int = 1
val res5: Int = 1
val res6: Int = 2
val res7: Int = 4
 */

// 2. "Number Orders"
def order(n: Int): Int =
  if(n > 0) (if(n % 3 == 0) 1 else 2) * (if(n == 50) 3 else 4) + (if(n % 7 == 0) 5 else 6) else 0

order(-1)
order(6)
order(50)
order(21)

/*
val res0: Int = 0
val res1: Int = 10
val res2: Int = 12
val res3: Int = 9
 */

// 3. "Number Species"
def species(n: Int) =
  if (0 < n) if (n % 2 == 0) 1 else 2 else 2

species(3)
species(6)
species(0)
species(-6)

/*
val res12: Int = 2
val res13: Int = 1
val res14: Int = 2
val res15: Int = 2
 */

// 4. Complete the Elbonian Tax Calculator
def tax(n: Double) = {
  n match {
    case n if n < 0 => throw new Exception("Invalid income!")
    case n if n < 20000 => 0.0
    case n if n < 30000 => n * 0.05
    case n if n < 40000 => n * 0.11
    case n if n < 60000 => n * 0.23
    case n if n < 100000 => n * 0.32
    case _ => n * 0.5
  }
}

tax(12300)
tax(29000)
tax(125000)
tax(1000000)
try {
  tax(-1000000)
} catch {
  case e: Exception => println(e)
}

/*
val res16: Double = 0.0
val res17: Double = 1450.0
val res18: Double = 62500.0
val res19: Double = 500000.0
java.lang.Exception: Invalid income!
val res20: AnyVal = ()
 */

// 5.
def drawRectangle(x: Int, y: Int): Unit = {
  for(i <- 0 until x) {
    for(j <- 0 until y) print('*')
    println()
  }
}

drawRectangle(3, 4)

drawRectangle(2,2)

/*
****
****
****

**
**
 */

// 6.
def printSums(x: Int, y: Int) =
  for(i <- 0 until x; j <- 0 until y) println(i + " + " + j + " = " + (i + j))

printSums(3, 4)

/*
0 + 0 = 0
0 + 1 = 1
0 + 2 = 2
0 + 3 = 3
1 + 0 = 1
1 + 1 = 2
1 + 2 = 3
1 + 3 = 4
2 + 0 = 2
2 + 1 = 3
2 + 2 = 4
2 + 3 = 5
 */

// 7. Continue and Break
//Scala BlackJack version 1
object BlackJack1 {
  import scala.util._
  import util.control.Breaks._

  val gen = new Random(System.currentTimeMillis())

  val cards = new Array[Int](52)
  for(i <- 0 until 52) cards(i) = if(gen.nextBoolean()) gen.nextInt(11) else -1

  var total = 0

  // iterate through cards incrementing total, use break to continue and break
  breakable {
    for(i <- 0 until 52) {
      breakable {
        if(cards(i) <= 0) break
        total += cards(i)
      }
      if(total >= 21) break
    }
  }

  println("total = " + total)
}

//Scala BlackJack version 2
object BlackJack2 {
  import scala.util._

  val gen = new Random(System.currentTimeMillis())

  val cards = new Array[Int](52)
  for(i <- 0 until 52) cards(i) = if(gen.nextBoolean()) gen.nextInt(11) else -1

  var total = 0

  // iterate through cards incrementing total, use break to continue and break
  try {
    for(i <- 0 until 52) {
      try {
        if(cards(i) <= 0) throw new Exception
        total += cards(i)
      } catch {
        case _: Throwable =>
      }
      if(total >= 21) throw new Exception
    }
  } catch {
    case _: Throwable =>
  }

  println("total = " + total)
}

//Scala BlackJack version 3
object BlackJack3 {
  import scala.util._

  val gen = new Random(System.currentTimeMillis())

  val cards = new Array[Int](52)
  for(i <- 0 until 52) cards(i) = if(gen.nextBoolean()) gen.nextInt(11) else -1

  var total = 0

  // iterate through cards incrementing total, use break to continue and break
  for(i <- 0 until 52 if total < 21) {
    if(cards(i) > 0) total += cards(i)
  }

  println("total = " + total)
}

BlackJack1
BlackJack2
BlackJack3

/*
total = 25
val res24: BlackJack1.type = BlackJack1$@4278c8ac
total = 23
val res25: BlackJack2.type = BlackJack2$@f75b249
total = 24
val res26: BlackJack3.type = BlackJack3$@6a48ea09
 */

// 8.
// = 1 if n belongs to realm 1(Odd positive), throws an exception otherwise
def realm1(n: Int) =
  if(n > 0) if(n % 2 != 0) 1
  else throw new Exception else throw new Exception

// = 2 if n belongs to realm 2(Even positives not divisible by 3), throws an exception otherwise
def realm2(n: Int) =
  if(n > 0) if(n % 2 == 0) if(n % 3 != 0) 2
  else throw new Exception else throw new Exception else throw new Exception

// = 3 if n belongs to realm 3(Positive multiples of both 6 and 7), throws an exception otherwise
def realm3(n: Int) =
  if(n > 0) if(n % 6 == 0) if(n % 7 == 0) 3
  else throw new Exception else throw new Exception else throw new Exception

// = the realm of n
def realm(n: Int) =
  try realm1(n) catch {
    case _: Throwable => try realm2(n) catch {
        case _: Throwable => try realm3(n) catch {
            case _: Throwable => 0
        }
      }
  }

realm(8)
realm(7)
realm(42)
realm(-9)

/*
val res27: Int = 2
val res28: Int = 1
val res29: Int = 3
val res30: Int = 0
 */

// 9.
def log(x: Double) = if(x <= 0) None else Some(math.log(x))

def sqrt(x: Double) = if(x < 0) None else Some(math.sqrt(x))

def sqrtLog(x: Double): Option[Double] =
  log(x) match {
    case None => None
    case Some(y) => sqrt(y)
  }

sqrtLog(0)
sqrtLog(10)
sqrtLog(100)
sqrtLog(-1)

/*
val res31: Option[Double] = None
val res32: Option[Double] = Some(1.5174271293851465)
val res33: Option[Double] = Some(2.145966026289347)
val res34: Option[Double] = None
 */

// Solutions
/*
package control

object control {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet\

  // =================
  // problem 1
  // =================
  def kingdom(n: Int) =
     if (n % 2 == 0)
       if (n > 10)
         if (n % 100 == 0) 1 else 2
       else 3
     else 4

  // =================
  // problem 2
  // =================
  def order(n: Int) =
     if (n <= 0) 0
     else
       (if (n % 3 == 0) 1 else 2) *
       (if (n == 50) 3 else 4) +
       (if (n % 7 == 0) 5 else 6)

  // =================
  // problem 3
  // =================

  //Dangling else, valueof species(0) isunspecified:

    species(0)  //> res10: AnyVal = ()

  //corrected version

  def species2(n: Int) =
  if (0 < n && n % 2 == 0) 1 else 2



  // =================
  // problem 4
  // =================

  class InvalidIncome extends Exception

  def tax(income: Double) =
     income match {
        case income if income < 0 => throw new InvalidIncome
        case income if income < 20000 => 0
        case income if income < 30000 => .05 * income
        case income if income < 40000 => .11 * income
        case income if income < 60000 => .23 * income
        case income if income < 100000 => .32 * income
        case _ => .5 * income
     }                                            //> tax: (income: Double)AnyVal

   tax(12300)                                     //> res0: AnyVal = 0
   tax(29000)                                     //> res1: AnyVal = 1450.0
   tax(125000)                                    //> res2: AnyVal = 62500.0
   tax(1000000)                                   //> res3: AnyVal = 500000.0
   try {
     tax(-1000000)
   } catch {
      case e: InvalidIncome => println("income must be non-negative")
      case _: Throwable => println("Serious error encontered")
   }                                              //> income must be non-negative
                                                  //| res4: AnyVal = ()

   // =================
  // problem 5
  // =================
  def drawRectangle(rows: Int, cols: Int) {
     for(row <- 0 until rows; col <- 0 until cols) {
        if (col == 0 && row != 0) print('\n')
        print('*')
     }
  }                                               //> drawRectangle: (rows: Int, cols: Int)Unit

  drawRectangle(3, 4)                             //> ****
                                                  //| ****
                                                  //| ****

  // =================
  // problem 6
  // =================
  def printSums(n: Int, m: Int) {
     for(i <- 0 until n; j <- 0 until m)
        println(i + " + " + j + " = " + (i + j))
  }                                               //> printSums: (n: Int, m: Int)Unit

  printSums(3, 4)                                 //> 0 + 0 = 0
                                                  //| 0 + 1 = 1
                                                  //| 0 + 2 = 2
                                                  //| 0 + 3 = 3
                                                  //| 1 + 0 = 1
                                                  //| 1 + 1 = 2
                                                  //| 1 + 2 = 3
                                                  //| 1 + 3 = 4
                                                  //| 2 + 0 = 2
                                                  //| 2 + 1 = 3
                                                  //| 2 + 2 = 4
                                                  //| 2 + 3 = 5

  // =================
  // problem 8
  // =================

  def realm1(n: Int) =
    if (n % 2 != 0) 1 else throw new Exception("not realm1")
                                                    //> realm1: (n: Int)Int

  def realm2(n: Int) =
    if (n % 2 == 0 && n % 3 != 0) 2 else throw new Exception("not realm2")
                                                    //> realm2: (n: Int)Int

  def realm3(n: Int) =
    if (n % 6 == 0 && n % 7 == 0) 3 else throw new Exception("not realm3")
                                                    //> realm3: (n: Int)Int


    def realm(n: Int) =
     try {
        realm1(n)
     } catch {
        case e: Exception =>
           try {
              println(e.getMessage) // diagnostic
              realm2(n)
           } catch {
              case e: Exception =>
                try {
                   println(e.getMessage) // diagnostic
                   realm3(n)
                } catch {
                   case e: Exception => {
                      println(e.getMessage) // diagnostic
                      0
                   }
                }
           }
   }

  realm(3)                                          //> res12: Int = 1
  realm(8)                                          //> not realm1
                                                    //| res13: Int = 2
  realm(6)                                          //> not realm1
                                                    //| not realm2
                                                    //| not realm3
                                                    //| res14: Int = 0
  realm(42)                                         //> not realm1
                                                    //| not realm2
                                                    //| res15: Int = 3
  realm(-1)                                         //> res16: Int = 1

  // =================
  // problem 9
  // =================

  def log(x: Double) = if (x <= 0) None else Some(math.log(x))
                                                  //> log: (x: Double)Option[Double]

  def sqrt(x: Double) = if (x < 0) None else Some(math.sqrt(x))
                                                  //> sqrt: (x: Double)Option[Double]

  def sqrtLog(x: Double) =
     log(x) match {
        case None => None
        case Some(y) => sqrt(y)
     }                                            //> sqrtLog: (x: Double)Option[Double]

  sqrtLog(0)                                      //> res5: Option[Double] = None

  sqrtLog(2.7)                                    //> res6: Option[Double] = Some(0.9966201748962759)


}
 */

// BlackJack
/*
import scala.util._
import util.control.Breaks._

object BlackJack extends App {

  val gen = new Random(System.currentTimeMillis())
  val cards = new Array[Int](52)
  for(i <- 0 until 52) cards(i) = if (gen.nextBoolean()) gen.nextInt(11) else -1

  var total = 0

  breakable {
    for(i <- 0 until 52) {
       breakable {
         println("cards(" + i + ") = " + cards(i))
         if (cards(i) <= 0) break
         total += cards(i)
         println("total = " + total)
       }
       if (21 <= total) break
    }
  }

  println("total = " + total)

}

object BlackJack2 extends App {

   val gen = new Random(System.currentTimeMillis())
   val cards = new Array[Int](52)
   for(i <- 0 until 52) cards(i) = if (gen.nextBoolean()) gen.nextInt(11) else -1

   var total = 0

   try {
     for(i <- 0 until 52) {
       try {
         if (cards(i) <= 0) throw new Exception
         total += cards(i)
       } catch {
         case _: Throwable =>
       }
       if (21 <= total) throw new Exception
     }
   } catch {
     case e: Exception =>
   }

   println("total = " + total)

}

object BlackJack3 extends App {
   val gen = new Random(System.currentTimeMillis())
   val cards = new Array[Int](52)
   for(i <- 0 until 52) cards(i) = gen.nextInt(10)

   var total = 0

   for(i <- 0 until 52 if total < 21) {
     if(cards(i) != 0) total += cards(i)
   }

   println("total = " + total)
}

 */
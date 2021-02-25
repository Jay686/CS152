import scala.io._
import scala.util.control.Breaks._

class SyntaxException(gripe: String = "ERROR") extends Exception(gripe)
class MissingOperator extends SyntaxException("Missing operator!")
class NumberFormat extends SyntaxException("Illegal operand!")


object peano {


  def execute(cmmd: String): String = {
    // if cmmd is a valid expression, compute its value and return it as a string
    def isNumeric(c: Char) = c.isDigit || c == '.'
    def isOperator(c: Char) = c == '+' || c == '*'
    var e = cmmd
    val num1Neg = e.charAt(0) == '-'
    if(num1Neg) e = e.drop(1)
    val num1 = e.takeWhile(isNumeric _)
    if(num1.isEmpty) throw new NumberFormat
    var num_1 = if(num1Neg) num1.toDouble * -1 else num1.toDouble
    e = e.drop(num1.length)
    val op = e.takeWhile(isOperator _)
    if(op.isEmpty) throw new MissingOperator
    e = e.drop(op.length)
    val num2Neg = e.charAt(0) == '-'
    if(num2Neg) e = e.drop(1)
    val num2 = e.takeWhile(isNumeric _)
    if(num2.isEmpty) throw new NumberFormat
    var num_2 = if(num2Neg) num2.toDouble * -1 else num2.toDouble
    if(op == "+") (num_1 + num_2).toString
    else if (op == "*") (num_1 * num_2).toString
    else Double.NaN.toString
  }


  // read-execute-print loop
  def repl {
    // repeatedly:
    //   1. prompt user for a string
    //   2. quit if cmmd == "quit"
    //   3. ignore if cmmd == ""
    //   4. print execute(cmmd) otherwise
    //   5. handle all exceptions
    println("Welcome to Peano 1.0")
    breakable {
      while(true) {
        var exp = StdIn.readLine("-> ")
        exp = exp.replaceAll(" ", "")
        exp = exp.replaceAll("\t", "")
        if(exp == "quit") {
          println("bye")
          break
        }
        breakable {
          if(exp == "") break
          try println(execute(exp))
          catch {
            case e: MissingOperator => println("Missing operator!\nPeano syntax: expression ::= operand~operator~operand\nwhere: operator ::= '+' | '*'")
            case e: NumberFormat => println("Illegal operand!\nPeano syntax: expression ::= operand~operator~operand\nwhere: operand ::= any valid floating point number")
          }
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    repl
  }
}

/*
Welcome to Peano 1.0
-> -2.4+8
5.6
->   -9+	1
-8.0
-> -2*5
-10.0
-> 1+2
3.0
-> 2*one
Illegal operand!
Peano syntax: expression ::= operand~operator~operand
where: operand ::= any valid floating point number
-> 2/8
Missing operator!
Peano syntax: expression ::= operand~operator~operand
where: operator ::= '+' | '*'
-> quit
bye

Process finished with exit code 0

 */

// Solutions
/*
import scala.io._

class SyntaxException(gripe: String = "ERROR") extends Exception(gripe)
class MissingOperator extends SyntaxException("Missing operator!")
class NumberFormat extends SyntaxException("Illegal operand!")


object console {


   def execute(cmmd: String): String = {
     val opPosition = Math.max(cmmd.indexOf('+'), cmmd.indexOf('*'))
     if (opPosition < 0) throw new MissingOperator
     try {
       val num1 = cmmd.take(opPosition).toDouble
       val num2 = cmmd.drop(opPosition + 1).toDouble
       cmmd.charAt(opPosition) match {
          case '+' => (num1 + num2).toString
          case '*' => (num1 * num2).toString
       }
     } catch {
       case e: NumberFormatException => throw new NumberFormat
     }
   }


   // read-execute-print loop
    def repl {
      var more = true
      var cmmd = ""
      println("Welcome to Peano 1.0")
      while(more) {
         try {
            print("-> ")
            cmmd = StdIn.readLine
            if (cmmd == "quit") more = false
            else if (cmmd != "") println(execute(cmmd))
         }
         catch {
           case e: MissingOperator => {
             println(e.getMessage)
             println("Peano syntax: expression ::= operand~operator~operand")
             println("where: operator ::= '+' | '*'")
           }
           case e: NumberFormat => {
             println(e.getMessage)
             println("Peano syntax: expression ::= operand~operator~operand")
             println("where: operand ::= any valid floating point number")
           }
           case e: SyntaxException => {
              println(e.getMessage)
            }
           case e: Exception => {
              println("A serious error has occurred, shutting down")
              println(e.getMessage)
              more = false
            }
         } finally {
            Console.flush
         }
      }
      println("bye")
   }

   def main(args: Array[String]): Unit = {
     repl
   }
}
 */
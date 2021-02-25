package atm

import scala.io.StdIn._

class ATMException extends Exception {}
class NegativeAmountException extends ATMException { }
class InsufficientFundsException extends ATMException { }
class BadCommandException extends ATMException { }

object atm {
  private var balance = 0.0
  
  def deposit(amt: Double) {
    if (amt < 0) throw new NegativeAmountException()
    balance += amt
  }
  
  def withdraw(amt: Double) {
    if (amt < 0) throw new NegativeAmountException
    if (balance < amt) throw new InsufficientFundsException
    balance -= amt
  }

  def repl {
    var more = true
    var amount = 0.0
    while(more) {
      try {
        print("-> ")
        var cmmd = readLine()
        if (cmmd == "deposit") {
          print("amount = $")
          amount = readDouble()
          deposit(amount)
          println("balance = $" + balance)
        } else if (cmmd == "withdraw") {
          print("amount = $")
          amount = readDouble()
          withdraw(amount)
          println("balance = $" + balance)
        } else if (cmmd == "quit") {
          more = false
        } else if (cmmd == "help") {
          println("Valid commands: withdraw, deposit, quit, and help");
        } else {
          throw new BadCommandException
        }
      } catch {
        case e:NegativeAmountException => println("amount must be positive")
        case e:InsufficientFundsException => println("Insufficient funds")
        case e: BadCommandException => println("Invalid command, type help")
        case e: ATMException => println("Command failed")
        case e: NumberFormatException => println("amount must be a number")
        case _: Throwable => {
          println("fatal error"); 
          more = false
          }
      } finally {
        println("...")
        Console.flush()
      }
    }
    println("bye")
  }
  
  def main(args: Array[String]): Unit = { repl }
}
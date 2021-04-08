abstract class Expression {
  def execute: Double
}

class Sum(val operand1: Expression, val operand2: Expression) extends Expression {
  def execute = operand1.execute + operand2.execute
  override def toString = "(+ " + operand1 + " " + operand2 + ")"
}

object Sum {
  def apply(operand1: Expression, operand2: Expression) =
    new Sum(operand1, operand2)
}

class Product(val operand1: Expression, val operand2: Expression) extends Expression {
  def execute = operand1.execute * operand2.execute
  override def toString = "(* " + operand1 + " " + operand2 + ")"
}

object Product {
  def apply(operand1: Expression, operand2: Expression) =
    new Product(operand1, operand2)
}

class Number(val v: Double) extends Expression {
  def execute = v
  override def toString = v.toString
}

object Number {
  def apply(v: Double) =
    new Number(v)
}

object TestAcorn extends App {
  var exp: Expression = Sum(Number(42), Product(Number(3.14), Number(2.71)))
  println("the value of " + exp + " = " + exp.execute)
  exp = Product(Number(2), Product(Number(3), Number(5)))
  println("the value of " + exp + " = " + exp.execute)
}
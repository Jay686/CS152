package Acron

class Number(val v: Double) extends Expression {
  def execute = v
  override def toString = v.toString
}

object Number {
  def apply(v: Double) =
    new Number(v)
}
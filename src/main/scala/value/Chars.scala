package value

import context.{Environment, TypeException}
import expression.Literal

case class Chars(val value: String) extends Addable with Ordered[Value] {
  override def +(other: Value): Chars =
    other match {
      case x: Addable => Chars(this.value + x.value)
      case _ => throw new TypeException("Arguments must be addable")
    }

  override def compare(other: Value): Int = {
    other match {
      //case x: Exact => this.value.toInt.compare(x.value)
      //case x: Inexact => this.value.toDouble.compare(x.value)
      case x: Chars => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }
  }

  def size(): Exact = Exact(value.length)

  def subChars(from: Exact, to: Exact): Chars =
    Chars(value.substring(from.value, to.value))

  override def equals(other: Any): Boolean =
    other match {
      case x: Chars => x.isInstanceOf[Chars] && x.value == this.value
      case _ => false
    }

  override def toString: String = value

  override def hashCode(): Int = value.hashCode()

  override def execute(env: Environment): Value = this
}

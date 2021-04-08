package value

import context.{Environment, TypeException}
import expression.Literal

case class Boole(val value: Boolean) extends Literal {

  def &&(other: Value): Boole =
    other match {
      case x: Boole => Boole(this.value && x.value)
      case _ => throw new TypeException("Boole operand required")
    }

  def ||(other: Value): Boole =
    other match {
      case x: Boole => Boole(this.value || x.value)
      case _ => throw new TypeException("Boole operand required")
    }

  def unary_!(): Boole = Boole(!value)

   override def equals(other: Any): Boolean =
    other match {
      case x: Boole => x.isInstanceOf[Boole] && x.value == this.value
      case _ => false
    }

  override def toString: String = value.toString

  override def execute(env: Environment): Value = this
}

object Boole {
  val FALSE = Boole(false)

  val TRUE = Boole(true)
}

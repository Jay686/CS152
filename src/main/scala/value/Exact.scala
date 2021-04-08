package value

import context.{Environment, IllegalValueException, TypeException}

case class Exact(val value: Int) extends Numeric with Ordered[Value] {

  def +(other: Value): Addable =
    other match {
      case x: Inexact => Inexact(this.value.toDouble + x.value)
      case x: Exact => Exact(this.value + x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  override def -(other: Value): Numeric =
    other match {
      case x: Inexact => Inexact(this.value.toDouble - x.value)
      case x: Exact => Exact(this.value - x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  override def *(other: Value): Numeric =
    other match {
      case x: Inexact => Inexact(this.value.toDouble * x.value)
      case x: Exact => Exact(this.value * x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  override def /(other: Value): Numeric =
    other match {
      case x: Inexact =>
        if(x.value == 0.0) throw new IllegalValueException("Divide by 0!")
        else Inexact(this.value.toDouble / x.value)
      case x: Exact =>
        if(x.value == 0) throw new IllegalValueException("Divide by 0!")
        else Exact(this.value / x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def unary_-(): Numeric = Exact(-this.value)

  override def compare(other: Value): Int = {
    other match {
      case x: Inexact => this.value.toDouble.compare(x.value)
      case x: Exact => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case x: Inexact => x.isInstanceOf[Inexact] && x.value == this.value.toDouble
      case x: Exact => x.isInstanceOf[Exact] && x.value == this.value
      case _ => false
    }
  }

  def ==(other: Value) = equals(other)

  override def toString: String = value.toString

  override def hashCode(): Int = value.toString.hashCode()

  override def execute(env: Environment): Value = this
}
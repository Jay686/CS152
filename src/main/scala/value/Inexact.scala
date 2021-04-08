package value

import context.{Environment, IllegalValueException, TypeException}

case class Inexact(val value: Double) extends Numeric with Ordered[Value] {

  override def +(other: Value): Addable =
    other match {
      case x: Inexact => Inexact(this.value + x.value)
      case x: Exact => Inexact(this.value + x.value.toDouble)
      case _ => throw new TypeException("Numeric operand required")
    }

  override def -(other: Value): Numeric =
    other match {
      case x: Inexact => Inexact(this.value - x.value)
      case x: Exact => Inexact(this.value - x.value.toDouble)
      case _ => throw new TypeException("Numeric operand required")
    }

  override def *(other: Value): Numeric =
    other match {
      case x: Inexact => Inexact(this.value * x.value)
      case x: Exact => Inexact(this.value * x.value.toDouble)
      case _ => throw new TypeException("Numeric operand required")
    }

  override def /(other: Value): Numeric =
    other match {
      case x: Inexact =>
        if(x.value == 0.0) throw new IllegalValueException("Divide by 0!")
        else Inexact(this.value / x.value)
      case x: Exact =>
        if(x.value == 0) throw new IllegalValueException("Divide by 0!")
        else Inexact(this.value / x.value.toDouble)
      case _ => throw new TypeException("Numeric operand required")
    }

  override def unary_-(): Numeric = Inexact(-this.value)

  override def compare(other: Value): Int =
    other match {
      case x: Inexact => this.value.compare(x.value)
      case x: Exact => this.value.compare(x.value.toDouble)
      case _ => throw new TypeException("Arguments must be comparable")
    }

  override def equals(other: Any): Boolean =
    other match {
      case x: Inexact => x.isInstanceOf[Inexact] && x.value == this.value
      case x: Exact => x.isInstanceOf[Exact] && x.value.toDouble == this.value
      case _ => false
    }

  def ==(other: Value) = equals(other)

  override def toString: String = value.toString

  override def hashCode(): Int = value.toString.hashCode()

  override def execute(env: Environment): Value = this
}

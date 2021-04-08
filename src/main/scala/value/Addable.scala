package value

import expression.Literal

trait Addable extends Literal {
  val value: Any
  def +(other: Value): Addable
}

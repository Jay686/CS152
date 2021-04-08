class Real(val scalaValue: Double) extends Ordered[Real] with Equals {
  def *(r: Real) = this.scalaValue * r.scalaValue

  override def <(that: Real): Boolean = super.<(that)

  override def toString: String = super.toString

  override def compare(that: Real): Int = if(this.scalaValue > that.scalaValue) 1 else 0

  override def canEqual(that: Any): Boolean = {
    if(that.isInstanceOf[Real]) true
    else false
  }
}
object Real {
  def apply(v: Double) = new Real(v)
}

object RealTest extends App {
  var r1 = Real(3.14)
  var r2 = Real(2.71)
  println("r1 * r2 = " + (r1 * r2))
  println("r1 == r2 = " + (r1 == r2))
  println("r1 < r2 = " + (r1 < r2))
}

/*
r1 * r2 = 8.5094
r1 == r2 = false
r1 < r2 = false
 */
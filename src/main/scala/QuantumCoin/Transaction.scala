package QuantumCoin

case class Transaction(val fromAccount: Int, val toAccount: Int, val amount: Double) {
  override def equals(other: Any) =
    other match {
      case t: Transaction => t.isInstanceOf[Transaction] &&
        t.toAccount == this.toAccount &&
        t.fromAccount == this.fromAccount &&
        t.amount == this.amount
      case _ => false
  }
  override def toString: String = "[" + fromAccount + ", " + toAccount + ", $" + amount + "]"

  override def hashCode(): Int = this.toString.hashCode
}
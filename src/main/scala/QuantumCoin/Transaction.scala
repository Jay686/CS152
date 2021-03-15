package QuantumCoin

class Transaction(val fromAccount: Int, val toAccount: Int, val amount: Double) {
  override def toString: String = "From " + " to " + toAccount + ":$" + amount

  override def hashCode(): Int = this.toString.hashCode
}

object Transaction {
  def apply(fromAcct: Int, toAcct: Int, amount: Double) = new Transaction(fromAcct, toAcct, amount)
}

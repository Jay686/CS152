package QuantumCoin

import scala.collection.mutable.ArrayBuffer

class Block {
  val transactions = new ArrayBuffer[Transaction]()
  var sconce: Int = 0
  val blockSize = 100
  var myHashCode: Int = -1

  def full = transactions.size == blockSize

  def add(t: Transaction) = {
    if (!full) transactions += t
  }

  def getBalance(acct: Int) = {
    val depos = transactions.filter(_.toAccount == acct)
    val withs = transactions.filter(_.fromAccount == acct)
    val depoTotal = if (depos != Nil) depos.map(_.amount).reduce(_ + _) else 0
    val withTotal = if (withs != Nil) withs.map(_.amount).reduce(_ + _) else 0
    depoTotal - withTotal
  }

  def upload() = {
    var success = false
    var tries = 0
    while(!success && tries < 50) {
      sconce += 1
      tries += 1
      println("sconce = " + sconce)
      success = BlockChain.add(this)
    }
    success
  }

  override def hashCode = {
    // includes hash codes of eachg transaction and the sconce
    var s = "" + sconce
    for(t <- transactions) {s = s + t.hashCode}
    myHashCode = math.abs(s.hashCode)
    myHashCode
  }
}
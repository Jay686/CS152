package QuantumCoin

import scala.collection.mutable.ArrayBuffer

class Block {
  private var _full = false
  private var sconce
  private val limit = 100
  private val transaction: ArrayBuffer[Transaction] = ArrayBuffer[Transaction]()

  def full = _full
  def add(t: Transaction) = ???

  def upload(): Any = ???

  def getBalance(acct: Int) = ???

  override def hashCode() = ???
}

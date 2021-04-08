package QuantumCoin

import scala.collection.mutable.ArrayBuffer

object BlockChain {
  private var control = 500000000;
  private val blocks = new ArrayBuffer[Block]()

  def getBalance(acct: Int) = {
    val bals = blocks.map(_.getBalance(acct))
    if (bals == Nil) 0.0 else bals.reduce(_ + _)
  }

  def add(b: Block) = {
    val bhc = b.hashCode
    println("block# = " + bhc)
    if(control < bhc ) false
    else {
      blocks += b
      true
    }
  }
}
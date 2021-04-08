import scala.collection.mutable._

case class Transaction(val from: Int, val to: Int, val amt: Double) {
  override def equals(other: Any): Boolean =
    other match  {
      case t: Transaction => t.isInstanceOf[Transaction] &&
        t.to == this.to &&
        t.from == this.from &&
        t.amt == this.amt
      case _ => false
    }
  override def toString: String = "[" + from + ", " + to + ", $" + amt + "]"
  override def hashCode(): Int = this.toString.hashCode
}

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
    val depos = transactions.filter(_.to == acct)
    val withs = transactions.filter(_.from == acct)
    val depoTotal = if (depos != Nil) depos.map(_.amt).reduce(_ + _) else 0
    val withTotal = if (withs != Nil) withs.map(_.amt).reduce(_ + _) else 0
    depoTotal - withTotal
  }

  def upload() = {
    var success = false
    var tries = 0
    while(!success && tries < 50) {
      sconce += 1
      tries += 1
      println("sconce = " + sconce)
      success = blockChain.add(this)
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

object blockChain {
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
object Miner extends App {

  var currentBlock: Block = new Block

  def addTransaction(t: Transaction) {
    currentBlock.add(t);
    if (currentBlock.full){
      println("trying to upload a block")
      println("upload success = " + currentBlock.upload())
      currentBlock = new Block;
    }
  }

  for(i <- 0 until 50) {
    addTransaction(Transaction(1, 2, 100.0)) // 1 pays $5000
    addTransaction(Transaction(2, 1, 20.0))  // 1 gets $1000
  }

  println("acct1 balance = $" + blockChain.getBalance(1))
  println("acct2 balance = $" + blockChain.getBalance(2))

}
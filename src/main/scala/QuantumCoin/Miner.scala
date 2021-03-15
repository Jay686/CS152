package QuantumCoin

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

  for(i <- 0 to 30) {
    addTransaction(Transaction(1, 2, 100.0))
    addTransaction(Transaction(2, 1, 20.0))

  }

  println("acct1 balance = $" + BlockChain.getBalance(1))
  println("acct2 balance = $" + BlockChain.getBalance(2))
}

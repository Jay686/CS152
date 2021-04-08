// 1.
case class Date(val d: Int, val m: Int, val y: Int)

case class Transaction(val value: Double, val date: Date, val credit: Boolean)

def debitsTR(ledger: List[Transaction], year: Int, r: Double = 0.0): Double = {
  if(ledger == Nil) r
  else debitsTR(ledger.tail, year, if(ledger.head.date.y == year && !ledger.head.credit) r + ledger.head.value else r)
}
def debits(ledger: List[Transaction], year: Int) =
  ledger.filter(_.date.y == year).filter(!_.credit).map(_.value).reduce(_ + _)

val ledger1 = List(
  Transaction(1200.00, Date(5, 12, 2020), false),
  Transaction(1750.00, Date(9, 15, 2020), true),
  Transaction(850.99, Date(10, 31, 2020), true),
  Transaction(1555.00, Date(11, 28, 2020), false),
  Transaction(2300.00, Date(12, 25, 2020), false),
  Transaction(1200.00, Date(1, 1, 2021), false)
)

debitsTR(ledger1, 2020)
debits(ledger1,2020)

/*
val res0: Double = 5055.0
val res1: Double = 5055.0
 */


// 2.
def g[S, T](x: Option[T], h: T => S) = {
  x match {
    case Some(n) => Some(h(n))
    case None => None
  }
}
def map[S, T](opt: List[Option[T]], f: T => S) = {
  opt.map(g(_, f))
}

val ops = List(Some(3), Some(5), None, Some(6))
map(ops, (x: Int) => x * x)

/*
val ops: List[Option[Int]] = List(Some(3), Some(5), None, Some(6))
val res2: List[Option[Int]] = List(Some(9), Some(25), None, Some(36))
 */

// 3.
trait AttackStrategy {
  def attack(opponent: Knight)
}

object Mace extends AttackStrategy {
  def attack(opponent: Knight) = {
    println("macing")
    opponent.health -= 10
  }
}

object Stab extends AttackStrategy {
  def attack(opponent: Knight) = {
    println("stabbing")
    opponent.health -= 15
  }
}

class CompositeStrategy(value: List[AttackStrategy]) extends AttackStrategy {
  override def attack(opponent: Knight) = {
    value.foreach(_.attack(opponent))
  }
}

class Knight(val name: String) {
  var health: Int = 100
  var strategy: AttackStrategy = null
  def attack(opponent: Knight) = {
    println(this.name + " is attacking " + opponent.name)
    strategy.attack(opponent)
    println(opponent.name + ".health = " + opponent.health)
  }
}

val k1 = new Knight("Drobot")
val k2 = new Knight("Baldimore")
k1.strategy = Mace
k2.strategy = new CompositeStrategy(List(Mace, Stab, Stab))

k1.attack(k2)
// Drobot is attacking Baldimore
// macing
// Baldimore.health = 90

k2.attack(k1)
// Baldimore is attacking Drobot
// macing
// stabbing
// stabbing
// Drobot.health = 60
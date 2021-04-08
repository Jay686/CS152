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

class Knight(val name: String, var health: Int = 100) {
  var strategy: AttackStrategy = null
  def attack(opponent: Knight) = {
    println(this.name + " is attacking " + opponent.name)
    strategy.attack(opponent)
    println(opponent.name + ".health = " + opponent.health)
  }
}

object test extends App {
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
}


//class Warrior(val name: String) {
//  var health = 100
//  var strategy: Warrior => Unit = _
//
//  def attack(opponent: Warrior) = {
//    println(name + " is attacking " + opponent.name)
//    strategy(opponent)
//    println(opponent.name + ".health = " + opponent.health)
//  }
//}
//def spitFire(opponent: Warrior): Unit = {
//  println("Spits fire")
//  opponent.health -= 10
//}
//
//def stomp(opponent: Warrior): Unit ={
//  println("Stomps")
//  opponent.health -= 7
//}
//def doNothing(w: Warrior) = {}
////Part B
//def compose(list: List[Warrior => Unit]) =
//  if (list.isEmpty) doNothing _
//  else (opponent: Warrior) =>
//    for (attack <- list) attack(opponent)
package DuelingGladiators

object Test extends App {

  val maximus = CrusherMasher("Optimus Prime")
  object bee extends Gladiator("Bumble Bee") with Slasher with Masher {
    override def attack(victim: Gladiator): Unit = {
      super.attack(victim)
      super.slash(victim)
      super.mash(victim)
    }
  }

  try {
    for(i <- 0 to 5) {
      maximus.attack(bee)
      bee.attack(maximus)
    }
  } catch {
  case e: GladiatorDeadException => println(e)
}

}

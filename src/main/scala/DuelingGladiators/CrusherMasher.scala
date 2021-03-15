package DuelingGladiators

class CrusherMasher(override val name: String) extends Gladiator(name) with Crusher with Masher {
  override def attack(victim: Gladiator): Unit = {
    super.attack(victim)
    super.crush(victim)
    super.mash(victim)
  }
}

object CrusherMasher {
  def apply(name: String) = new CrusherMasher(name)
}

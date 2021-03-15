package DuelingGladiators

trait Masher {
  def mash(victim: Gladiator) = {
    if(victim.health > 0) {
      printf(f"${victim.name} is being mashing...\n")
      if (5 > victim.health) victim.damage(victim.health)
      else victim.damage(5)
    }
  }
}

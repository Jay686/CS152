package DuelingGladiators

trait Slasher {
  def slash(victim: Gladiator) = {
    if(victim.health > 0) {
      printf(f"${victim.name} is being slashing...\n")
      if(5 > victim.health) victim.damage(victim.health)
      else victim.damage(5)
    }
  }
}

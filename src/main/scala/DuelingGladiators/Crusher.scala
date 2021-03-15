package DuelingGladiators

trait Crusher {
  def crush(victim: Gladiator) = {
    if(victim.health > 0) {
      printf(f"${victim.name} is being crushing...\n")
      if(5 > victim.health) victim.damage(victim.health)
      else victim.damage(5)
    }
  }
}

package DuelingGladiators

class GladiatorDeadException(message: String) extends Exception(message) {}

class Gladiator(val name: String, var health: Int = 100) {
/*  // getter
  def health = _health
  // setter, Gladiator.health = newVal complies to Gladiator_=(newVal)
  def health_=(h: Int) = {
    if (h >= 0 && h <= 100) _health = h
    else throw new IllegalArgumentException
  }*/

  def damage(d: Int) = {
    if(d <= health) health = health - d
    else throw new IllegalArgumentException
  }

  def attack(victim: Gladiator) = {

    if(this.health > 0 && victim.health > 0) {
      printf(f"${this.name} attacking ${victim.name}...\n")
      var loss = (math.random() * 100).toInt % this.health
      if(loss > victim.health) loss = victim.health
      victim.damage(loss)
    }
    else if(this.health <= 0)
      throw new GladiatorDeadException(f"${this.name} already dead!")
    if(victim.health <= 0)
      throw new GladiatorDeadException(f"${victim.name} already dead!")

  }

}

object Gladiator {
  def apply(name: String, health: Int = 100) = new Gladiator(name, health)
}

package time1

class Time(val hours: Int, val minutes: Int = 0) {
  if(hours < 0 || hours > 23 || minutes < 0 || minutes > 59) throw new IllegalArgumentException
  def minutesSinceMidNight: Int = hours * 60 + minutes
  def before(other: Time): Boolean = this.minutesSinceMidNight < other.minutesSinceMidNight
  override def toString: String = f"$hours:$minutes%02d"
}

object Time {
  def apply(hours: Int, minutes: Int = 0) = new Time(hours, minutes)
}

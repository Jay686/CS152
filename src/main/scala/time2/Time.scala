package time2

class Time(private var _hours: Int, private var _minutes: Int = 0) {
  // getter
  def hours = _hours
  // setter, Time.hours = newVal complies to hours_=(newVal)
  def hours_=(h: Int) = {
    if(h > 0 && h < 23) _hours = h
    else throw new IllegalArgumentException
  }

  // getter
  def minutes = _minutes
  // setter, Time.minutes = newVal complies to minutes_=(newVal)
  def minutes_=(m: Int) = {
    if(m > 0 && m < 59) _minutes = m
    else throw new IllegalArgumentException
  }

  def minutesSinceMidNight: Int = _hours * 60 + _minutes
  def before(t: Time): Boolean = minutesSinceMidNight < t.minutesSinceMidNight
  override def toString: String = {
    def h = _hours
    def m = _minutes
    f"$h:$m%02d"
  }
}

object Time {
  def apply(hours: Int, minutes: Int = 0) = new Time(hours, minutes)
}

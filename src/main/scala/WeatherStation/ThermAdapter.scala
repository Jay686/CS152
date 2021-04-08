package WeatherStation

class ThermAdapter extends IThermometer {
  override def getMeanTemperature(cities: List[String]): Double = {
    val celTherm: CelsiusTherm = new CelsiusTherm
    def celToFah(c: Double) = c * 9 / 5 + 32
    cities.map(celTherm.computeTemp).map(celToFah).map(_ / cities.length).reduce(_ + _)
  }
}
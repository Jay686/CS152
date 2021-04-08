package WeatherStation

object WeatherStation extends App {
  val thermometer: IThermometer = new ThermAdapter
  val avgTemp =
    thermometer.getMeanTemperature(List("LA", "SF", "SLC", "Rio"))
  println("avg temp = " + avgTemp)
}
/*
avg temp = 85.70364257729634
 */
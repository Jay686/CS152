class Calculator {
  protected var result = 0.0
  def add(x: Double) { result += x }
  def mul(x: Double) { result *= x }
  def div(x: Double) { result /= x }
  def sub(x: Double) { result -= x }
  def clear { result = 0.0 }
  def getResult = result
  def help {
    println("Commands: add, mul, div, sub, clear, getResult, help")
  }
}

class MathCalculator extends Calculator {
  def sin { result = math.sin(result) }
  def cos { result = math.cos(result) }
  def tan { result = math.tan(result) }
  override def help {
    super.help
    println("Also: sin, cos, tan")
  }
}



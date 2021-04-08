// problem 1

// commands as functions version

def accum(program: List[Int=>Int]) = {
  var reg = 0
  for(cmmd <- program) reg = cmmd(reg)
  reg
}

def add(n: Int) = (reg: Int) => reg + n
def mul(n: Int) = (reg: Int) => reg * n
def display(reg: Int) = {println("reg = " + reg); reg}
def clear(reg: Int) = 0

val program = List(add(3), mul(4), add(5), display _, clear _, add(9), mul(2))

accum(program)

// Commands as Objects version

trait Instruction {
  def execute
}

object accum {
  var program: List[Instruction] = Nil
  var register = 0
  var halt = false
  var ip = 0
  def run: Unit = {
    //for(inst <- program if !halt) inst.execute
    ip = 0
    while(ip < program.length && !halt) {
      program(ip).execute
      ip += 1
    }
  }
}

case class Add(val n: Int) extends Instruction {
  def execute = {
    accum.register += n
  }
}

case class Mul(val n: Int)  extends Instruction {
  def execute = {
    accum.register *= n
  }
}

case class Halt()  extends Instruction {
  def execute = {
    accum.halt = true
  }
}

case class Goto(val n: Int)  extends Instruction {
  def execute = {
    accum.ip = n - 1
  }
}

accum.program = List(Add(3), Mul(5), Goto(5), Add(1), Halt(), Mul(2))
accum.run
accum.register

// problem 2
def pipe[T, S](f: T=>S, g: T=>S): T=>S = {
  def h(t: T): S =
    try {
      f(t)
    } catch {
      case e: Exception => g(t)
    }
  h _
}                                               //> pipe: [T, S](f: T => S, g: T => S)T => S

val toInteger = pipe((s: String) => s.toInt, (s: String)=>0)
//> toInteger  : String => Int = <function1>

toInteger("12345")                              //> res3: Int = 12345
toInteger("123x45")                             //> res4: Int = 0


// problem 3
def isPal(s: String) = s == s.reverse           //> isPal: (s: String)Boolean
def length(s: String) = s.size                  //> length: (s: String)Int
def max(n: Int, m: Int) = if(n < m) m else n    //> max: (n: Int, m: Int)Int

// map-reduce solution
def maxPal1(words: List[String]): Int =
  words.filter(isPal _).map(length _).reduce(max _)
//> maxPal1: (words: List[String])Int

maxPal1(List("mom", "rotator", "cowbells", "dad"))
//> res5: Int = 7
// tail recursive solution
def maxPal2(words: List[String]): Int = {
  def helper(result: Int, unseen: List[String]): Int =
    if (unseen == Nil) result
    else helper(if (isPal(unseen.head)) max(length(unseen.head), result) else result, unseen.tail)
  helper(0, words)
}                                               //> maxPal2: (words: List[String])Int

maxPal2(List("mom", "rotator", "cowbells", "dad"))

// problem 4
case class Entry(val hour: Int, val temp: Double) {
  override def toString = "[" + hour + ":00 temp = " + temp + " degs]"
}

def cel2far(celTemp: Double): Double = 9 * celTemp / 5 + 32

def logProcessor(entries: List[Entry]) =
  entries.filter(_.hour <= 17).map((e: Entry) => Entry(e.hour,cel2far(e.temp)))

val readings = List(Entry(6, 25), Entry(10, 28), Entry(12, 32), Entry(16, 30), Entry(18, 26), Entry(22, 19))

logProcessor(readings)

// Problem 5: Color
class Color(val red: Int, val green: Int, val blue: Int) {

  if (red < 0 || green < 0 || blue < 0)
    throw new Exception("Color components pust be positive")
  if (240 < red || 240 < green || 240 < blue)
    throw new Exception("Color components pust be <= 240")

  override def equals(other: Any) =
    other match {
      case c: Color => c.isInstanceOf[Color] &&
        c.green == this.green &&
        c.red == this.red &&
        c.blue == this.blue
      case _ => false
    }


  override def toString = "Color(" + red + ", " + green + ", " + blue + ")"

  override def hashCode = this.toString.hashCode

}

object Color {
  def apply(red: Int, green: Int, blue: Int) = new Color(red, green, blue)
  val RED = new Color(240, 0, 0)
  val GREEN = new Color(0, 240, 0)
  val BLUE = new Color(0, 0, 240)
}

val purple1 = Color(240, 0, 240)
val purple2 = Color(240, 0, 240)


purple1 == purple2

val colorNames = Map(purple1 -> "Purple", Color.GREEN -> "Green")
colorNames(purple2)

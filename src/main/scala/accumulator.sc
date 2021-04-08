
trait Instruction {
  def execute(reg: Int): Int
}

class Add(arg: Int) extends Instruction {
  def execute(reg: Int) = reg + arg
}
object Add {
  def apply(arg: Int) = new Add(arg)
}

class Mul(arg: Int) extends Instruction {
  def execute(reg: Int)= reg * arg
}
object Mul {
  def apply(arg: Int) = new Mul(arg)
}


object Accumulator {
  var program: List[Instruction] = Nil
  var register = 0

  var HALT = false
  var IP = 0

  def run() = {
    while(IP < program.length && !HALT) {
      register = program(IP).execute(register)
      IP += 1
    }
    IP = 0
  }
}

case class Halt() extends Instruction {
  def execute(reg: Int) = {
    Accumulator.HALT = true
    reg
  }
}

case class Goto(arg: Int) extends Instruction {
  def execute(reg: Int)= {
    Accumulator.IP = arg - 1
    reg
  }
}


// computing ((3 * 5) + 1) * 2
Accumulator.program = List(Add(3), Mul(5), Add(1), Mul(2))
Accumulator.run()
Accumulator.register                           //> res6: Int = 32
// computing (((10 * 2) + 3) * 5)
Accumulator.register = 0
Accumulator.program = List(Add(10), Mul(2), Add(3), Mul(5))
Accumulator.run()
Accumulator.register                           //> res7: Int = 115
// computing (10 * 5)
Accumulator.register = 0
Accumulator.program = List(Add(10), Goto(3), Add(3), Mul(5))
Accumulator.run()
Accumulator.register
// computing (10 * 2)
Accumulator.register = 0
Accumulator.program = List(Add(10), Mul(2), Halt(), Mul(5))
Accumulator.run()
Accumulator.register

/*
// mutated Accumulator.program

val res1: Int = 32

// mutated Accumulator.register
// mutated Accumulator.program

val res3: Int = 115

// mutated Accumulator.register
// mutated Accumulator.program

val res5: Int = 50

// mutated Accumulator.register
// mutated Accumulator.program

val res7: Int = 20
 */
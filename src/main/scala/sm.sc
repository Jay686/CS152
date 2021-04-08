import scala.collection.mutable.ArrayBuffer

class NotEnoughElementException(message: String) extends Exception(message) {}

trait Command {
  def execute(s: ArrayBuffer[Int]): Any
}

case class Push(arg: Int) extends Command {
  def execute(s: ArrayBuffer[Int]) =
    s.insert(0, arg)
}

case class Pop() extends Command {
  def execute(s: ArrayBuffer[Int]) = {
    if(s == Nil || s.length < 1)
      throw new NotEnoughElementException("At least 1 element is needed")
    else {
      var head = s.head
      s.remove(0)
      head
    }
  }
}

case class Top() extends Command {
  def execute(s: ArrayBuffer[Int]) = {
    if(s == Nil || s.length < 1)
      throw new NotEnoughElementException("No element")
    else println(s.head)
  }
}

case class Sum() extends Command {
  def execute(s: ArrayBuffer[Int]) = {
    if(s == Nil || s.length < 2)
      throw new NotEnoughElementException("At least 2 elements is needed")
    else {
      var first = s.head
      s.remove(0)
      var second = s.head
      s.remove(0)
      s.insert(0, first + second)
    }
  }
}

case class Times() extends Command {
  def execute(s: ArrayBuffer[Int]) = {
    if(s == Nil || s.length < 2)
      throw new NotEnoughElementException("At least 2 elements is needed")
    else {
      var first = s.head
      s.remove(0)
      var second = s.head
      s.remove(0)
      s.insert(0, first * second)
    }
  }
}

object StackMachine {
  var stack = new ArrayBuffer[Int]()
  var program: List[Command] = Nil
  def run() = {
    try {
      for(comm <- program) comm.execute(stack)
    }
    catch {
      case e: NotEnoughElementException => println(e)
    }
  }
}

StackMachine.program = List(Push(3), Push(4), Push(5), Sum(), Times(), Top())
StackMachine.run()                             //> top = 27

StackMachine.program = List(Push(10), Push(10), Times(), Push(20), Sum(), Top())
StackMachine.run()                             //> top = 120

StackMachine.stack.clear()
StackMachine.program = List(Top())
StackMachine.run()
StackMachine.program = List(Sum(), Top())
StackMachine.run()
StackMachine.program = List(Times(), Top())
StackMachine.run()
StackMachine.program = List(Pop(), Top())
StackMachine.run()

/*
// mutated StackMachine.program
27

// mutated StackMachine.program
120


// mutated StackMachine.program
$line2.$read$$iw$NotEnoughElementException: No element
// mutated StackMachine.program
$line2.$read$$iw$NotEnoughElementException: At least 2 elements is needed
// mutated StackMachine.program
$line2.$read$$iw$NotEnoughElementException: At least 2 elements is needed
// mutated StackMachine.program
$line2.$read$$iw$NotEnoughElementException: At least 1 element is needed
 */
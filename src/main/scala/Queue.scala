import scala.collection.mutable.ArrayBuffer

class Queue[T] {
  private var arr = new ArrayBuffer[T]()

  def enqueue(elem: T) = arr.append(elem)
  def dequeue = {
    var h = arr.head
    arr = arr.tail
    h
  }
  def front = arr.head
  def isEmpty = arr.isEmpty
  override def toString =
    if(arr == Nil) "Nil"
    else arr.mkString("Queue[", ", ", "]")
}

object Queue {
  def apply[T](elems: T*) = {
    var q = new Queue[T]
    for(elem <- elems) q.enqueue(elem)
    q
  }
  def test1() {
    val wait = Queue()
    val waitingList = Queue[String]("Sid", "Barb", "Joel")
    waitingList.enqueue("Jim")
    waitingList.enqueue("Ken")
    waitingList.enqueue("Jake")
    waitingList.enqueue("Mark")
    waitingList.enqueue("Leo")
    println(wait)
    println(waitingList)
    println(waitingList.front)
    while(!waitingList.isEmpty) println(waitingList.dequeue)
  }

}

object tester extends App {
  Queue.test1
}

/*
Nil
Queue[Sid, Barb, Joel, Jim, Ken, Jake, Mark, Leo]
Sid
Sid
Barb
Joel
Jim
Ken
Jake
Mark
Leo
 */
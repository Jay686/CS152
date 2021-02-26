// this is how one declares a class with three fields in Scala:
case class Note(val amplitude: Double, val frequency: Double, val duration: Double = 1.0)

// a sample score for testing purposes:
val symphony1 =
  List(Note(3, 30), Note(3.1, 40, .25), Note(3.2, 10, .5), Note(5.1, 5, -.75), Note(3.9, 2))

// Iterative Solution
def duration(score: List[Note]) = {
  var result = 0.0
  for(note <- score if 0 < note.duration)
    result = result + note.duration
  result
}

// Recursive Solution
def duration(score: List[Note]):Double =
  if (score == Nil) 0.0
  else if (0 < score.head.duration) score.head.duration + duration(score.tail)
  else duration(score.tail)

// Tail Recursive Solution
def duration(score: List[Note]) = {
  def helper(result: Double, unseen: List[Note]): Double =
    if (unseen == Nil) result
    else if  (0 < unseen.head.duration)
      helper(result + unseen.head.duration, unseen.tail)
    else helper(result, unseen.tail)
  helper(0.0, score)
}

// Pipeline Solution
def sum(a: Double, b: Double) = a + b
def getDuration(n: Note) = n.duration
def isPositive(dur: Double) = 0 < dur
def  duration(score: List[Note])
= score.map(getDuration).filter(isPositive).reduce(sum)
// Or
def  duration(score: List[Note])
= score.map(_.duration).filter(0 < _).reduce(_ + _)

def maxAmp(score: List[Note]) =
  score.filter(_.duration > 0).map(_.amplitude).reduce(math.max(_, _))

maxAmp(symphony1)
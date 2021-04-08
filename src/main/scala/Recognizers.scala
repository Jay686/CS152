trait Recognizers {

  // matches(s) = s
  def matches(s: String): String => Boolean = {
    def r(s1: String) = s1.trim.equalsIgnoreCase(s)
    r _
  }

  // opt(r) = r?
  def opt(r: String => Boolean): String => Boolean = {
    def f(s: String) =
      if(s == "") true else r(s)
    f _
  }

  // pipe(r1, r2) = r1 | r2
  def pipe(r1: String => Boolean, r2: String => Boolean) : String => Boolean = {
    def f(s: String) = r1(s) || r2(s)
    f _
  }

  // follows(r1, r2) = r1 ~ r2
  def follows(r1: String => Boolean, r2: String => Boolean) : String => Boolean = {
    def f(s: String) = {
      if (s == "") false
      else {
//        var result2 = false
//        var j = 0
//        while(!result2 && j < s.length) {
//          result2 = r1(s.take(j))
//          j += 1
//        }
//        result2 = result2 && r2(s.drop(j - 1))
//
//        var result1 = false
//        var i = 0
//        while(!result1 && i < s.length) {
//          result1 = r2(s.drop(i))
//          i += 1
//        }
//        result1 = result1 && r1(s.take(i - 1))
//
//        result1 || result2
        var result = false
        for(i <- 0 to s.length if !result)
          result = r1(s.take(i)) && r2(s.drop(i))
        result
      }
    }
    f _
  }

//  def follows(r1: String => Boolean, r2: String => Boolean) : String => Boolean = {
//    def res(s1: String) = {
//      var result = false
//      var index = 0
//      for(i <- 0 until s1.size if !result) {
//        result = r1(s1.substring(0, i))
//        index = i
//      }
//      r2(s1.substring(index))
//    }
//    res _
//  }

  //rep(r) = r*
  def rep(r: String => Boolean): String => Boolean = {
    //r1 uses recursion and iteration!
    def r1(s: String): Boolean = {
      var result = false
      if (s == "") result = true
      else {
        for(i <- 0 to s.length if !result)
          result = r(s.take(i)) && r1(s.drop(i))
      }
      result
    }
    r1 _
  }
//  def rep(r: String => Boolean): String => Boolean = {
//    //r1 uses recursion and iteration!
//    def r1(s: String): Boolean = {
//      var result = false
//      if (s == "") result = true
//      else {
//        var i = 0
//        while(i < s.length && !result) {
//          result = r(s.take(i))
//          i += 1
//        }
//        var j = i + i
//        while(j < s.length && result) {
//          result = r(s.take(j).drop(i))
//          j += i
//          i += i
//        }
//      }
//      result
//    }
//    r1 _
//  }
}


object RecognizerTests extends App with Recognizers {

  // exp1 ::= 00 ~ 11 | 111
  def exp1 = pipe(follows(matches("00"), matches("11")), matches("111"))

  println(exp1(""))
  println(exp1("0011")) // = true
  println(exp1("111"))  // = true
  println(exp1("000011")) // = false, too many 0's

  // exp2 ::= (00)* ~ (111) ~ (00)?
  var exp2 = follows(follows(rep(matches("00")), matches("111")), opt(matches("00")))
  println(exp2(""))
  println(exp2("0000111")) // = true
  println(exp2("00000011100")) // = true
  println(exp2("00011100")) // = false, #leading zeroes must be even
  println(exp2("11100")) // = true
}
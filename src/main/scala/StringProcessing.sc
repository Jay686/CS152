// String Processing

// 1.
def isPal(s: String) = {
  var s1 = s
  var s2 = s
  s1 = s1.dropWhile(_ == ' ')
  s2 = s1.reverse.dropWhile(_ == ' ')
  s1 == s2
}

isPal("  rotator")
isPal("cat ")
isPal("Civic")
isPal("Toyota")
isPal("$3.1441.3$")

/*
val res0: Boolean = true
val res1: Boolean = false
val res2: Boolean = false
val res3: Boolean = false
val res4: Boolean = true
 */

// 2.
def isPal2(s: String) = {
  var s1 = s
  s1 = s1.replaceAll("""[\p{Punct}]""", "")
  s1 = s1.filterNot(_ == ' ')
  s1 = s1.toLowerCase()
  isPal(s1)
}

isPal2("A man, a plan, a canal, .Panama!")
isPal2("A man, a plan, a canal, Panama!")
isPal2("A man, a plan, a caal, Panama!")

/*
val res5: Boolean = true
val res6: Boolean = true
val res7: Boolean = false
 */

// 3.
import scala.util.Random
def mkWord(size: Int = 5) = {
  val gen = new Random(System.currentTimeMillis())
  var s = ""
  for(i <- 0 until size) s += (gen.nextInt(26)+97).toChar
  s
}

mkWord(3)
mkWord(7)
mkWord()
mkWord()

/*
val res8: String = phz
val res9: String = hpcfuzu
val res10: String = ltztb
val res11: String = dmbou
 */

// 4.
import scala.util.Random
def mkSentence(size: Int = 10) = {
  val gen = new Random()
  var s = (gen.nextInt(26)+65).toChar.toString
  for(i <- 0 until 9) s += (gen.nextInt(26)+97).toChar
  for(i <- 0 until (size - 1)) s += (' ' + mkWord(gen.nextInt(10) + 1))
  s += '.'
  s
}

mkSentence()
mkSentence()
mkSentence()
mkSentence(5)

/*
val res12: String = Eqcebihzto yfrgavqqi yfrg yfrgav y yfrga y yfrgavqqi yfrgav y.
val res13: String = Dyfgyrgzbq jhk jhkrvpr jhk jhkrvpry jhk j jh jhkrvpryz jhkrvp.
val res14: String = Dlfximvffi plrghkejho plrgh plrghkej plrghkejho p pl plrghkejho plrg plrg.
val res15: String = Uncpmnyhpq msbua msbu msbuay msbuayfaqf.
 */

// Solutions
/*
object strings {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

      //+++++++++++++++++
  // Problem 1

  def isPal(word: String) = word.equals(word.reverse)
                                                  //> isPal: (word: String)Boolean

  isPal("rotator")                                //> res0: Boolean = true
  isPal("cat")                                    //> res1: Boolean = false
  isPal("Civic")                                  //> res2: Boolean = false
  isPal("Toyota")                                 //> res3: Boolean = false

  //+++++++++++++++++
  // Problem 2

  def isPal2(phrase: String) = {
     def filterAlphaNums(str: String) = {
        var result = ""
        for(c <- str) if (c.isLetterOrDigit) result += c
        result
     }
     isPal(filterAlphaNums(phrase.toLowerCase))
  }                                               //> isPal2: (phrase: String)Boolean

  isPal2("Civic")                                 //> res4: Boolean = true
  isPal2("Doc note, I dissent. A fast never prevents a fatness. I diet on cod!")
                                                  //> res5: Boolean = true
  isPal2("Who is on first?")                      //> res6: Boolean = false

  //+++++++++++++++++
  // Problem 3

  def mkPal(phrase: String) = phrase + phrase.reverse
                                                  //> mkPal: (phrase: String)String

  isPal2(mkPal("Batman and Robin"))               //> res7: Boolean = true

  //+++++++++++++++++
  // Problem 4

  val generator = new util.Random(System.currentTimeMillis)
                                                  //> generator  : scala.util.Random = scala.util.Random@39ba5a14

  def mkWord(size: Int = 5) = {
     var result = ""
     for(i <- 0 until size) result = result + ('a' + generator.nextInt(26)).toChar
     result
  }                                               //> mkWord: (size: Int)String

 mkWord()                                         //> res8: String = mlnwi
 mkWord(20)                                       //> res9: String = amzcdivnwjgvpwkvusud
 mkWord(5)                                        //> res10: String = hdfbk
 mkWord()                                         //> res11: String = koykf

  //+++++++++++++++++
  // Problem 5

  def mkSentence(size: Int = 10) = {
      var result  = ('A' + generator.nextInt(26)).toChar + mkWord(generator.nextInt(15))
      for(i <- 0 until size) result = result + ' ' + mkWord(generator.nextInt(15))
      result + '.'
  }                                               //> mkSentence: (size: Int)String

  mkSentence(6)                                   //> res12: String = Frzbipywucm qvvkdtrjsvafwn imgnd eiagd zhupok oaiwfniybdd z
                                                  //| fzoqvsolrxt.
  mkSentence()                                    //> res13: String = Uwoaizibzyxqsgl hjz dnontqaqyho dni heyriquskhkmiy enprak w
                                                  //| zaaj optdjk fov jrqiso kdnvviaaoq.

 */
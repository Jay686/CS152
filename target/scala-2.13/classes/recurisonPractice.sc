//pow1(n: Int) = 2n (you may assume n >= 0, use classical recursion)
//pow2(n: Int) = 2n (you may assume n >= 0, use tail recursion)
//count1(a: Char, s: String) = # of occurrences of a in s, use classical recursion
//count2(a: Char, s: String) = # of occurrences of a in s, use tail recursion


def pow1(n: Int): Int = if (n == 0) 1 else 2 * pow1(n - 1)


def pow2(n: Int) = {
  def helper(count: Int, result: Int): Int =
    if (n <= count) result else helper(count + 1, result * 2)
  helper(0, 1)
}


def contains1(a: Char, s: String): Boolean =
  if (s == "") false else s(0) == a || contains1(a, s.drop(1))


def contains2(a: Char, s: String) = {
  def helper(unseen: String, result: Boolean): Boolean =
    if (unseen == "" || result) result else helper(unseen.drop(1), unseen(0) == a)
  helper(s, false)
}

def count1(a: Char, s: String): Int =
  if(s == "") 0 else (if (s(0) == a) 1 else 0) + count1(a, s.drop(1))

def count11(a: Char, s: String): Int =
  if(s == "") 0
  else
    if (s(0) == a) 1 + count11(a, s.drop(1))
    else count11(a, s.drop(1))


def count2(a: Char, s: String) = {
  def helper(unseen: String, result: Int): Int =
    if (unseen == "") result
    else if (unseen(0) == a)
      helper(unseen.drop(1), result + 1)
    else helper(unseen.drop(1), result)

  helper(s, 0)
}